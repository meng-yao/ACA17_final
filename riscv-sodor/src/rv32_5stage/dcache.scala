package object AcaCustom
{
    import Chisel._    
    import Node._
    import Sodor.Constants._
    import Common._

    class DCacheInterface()(implicit conf: SodorConfiguration) extends Module
    {
        val io = new Bundle {
            val core_port = (new MemPortIo(data_width=conf.xprlen)).flip
            val mem_port = new MemPortIo(data_width=conf.xprlen)
        }
    }

    class NoDCache()(implicit conf: SodorConfiguration) extends DCacheInterface
    {
        io.mem_port <> io.core_port
    }

    class NoDCache2()(implicit conf: SodorConfiguration) extends DCacheInterface
    {
        // Extract desired data from burst
        val burst_len = 64    // 64 byte per line
        val burst_len_bit = 6 // 2^6 = 64
        val word_len = 4      // 4 byte per word
        val word_len_bit = 2  // 2^2 = 4

        val req_valid      = io.core_port.req.valid
        val req_addr       = io.core_port.req.bits.addr
        val req_data       = io.core_port.req.bits.data
        val req_fcn        = io.core_port.req.bits.fcn
        val req_typ        = io.core_port.req.bits.typ
        
        val req_valid_reg      = Reg(Bool())
        val req_addr_reg       = Reg(Bits())
        val req_data_reg       = Reg(Bits())         
        val req_fcn_reg        = Reg(Bits())
        val req_typ_reg        = Reg(Bits())
        val index_reg          = Reg(Bits())
        
        val burst_data     = io.mem_port.resp.bits.burst_data
        val word_idx_in_burst = req_addr(burst_len_bit - 1, word_len_bit)
        val word_data_ori = 
            Mux1H(UIntToOH(word_idx_in_burst, width=(burst_len / word_len)), burst_data)

        val byte_idx_in_word = req_addr(word_len_bit - 1, 0)
        val read_data_ori = LoadDataGen(word_data_ori >> (byte_idx_in_word << 3), io.core_port.req.bits.typ)
        when(io.mem_port.resp.valid){
            //for(i <- 0 until 16)
                //printf("burst data %d: %x\n", UInt(i),burst_data(i))
            
            //printf("offset:%x ,read data ori: %x\n",word_idx_in_burst, read_data_ori)
        }
        
        // Wiring
        io.mem_port.req.valid := Bool(false)
        io.mem_port.req.bits.addr := Bits(0)
        io.mem_port.req.bits.data := Bits(0)
        io.mem_port.req.bits.fcn := Bits(0)
        io.mem_port.req.bits.typ := Bits(0)

        val DCACHE_ENTRIES = 1024
        val DCACHE_ENTRIES_BIT = 10
        val DCACHE_TAG_BIT = conf.xprlen-DCACHE_ENTRIES_BIT-burst_len_bit
        val DCACHE_BITS = 1+DCACHE_TAG_BIT+burst_len*8
        //valid bit + tag + data bit
        
        val dcache = Mem(Bits(width=DCACHE_BITS), DCACHE_ENTRIES)
        val dcache_write_addr = UInt(width=DCACHE_ENTRIES_BIT)
        val dcache_write_en = Bool()
        val dcache_write_data = UInt(width=DCACHE_BITS)
        val dcache_read_addr = UInt(width=DCACHE_ENTRIES_BIT)
        dcache_write_addr := UInt(0)
        dcache_write_en := Bool(true)
        dcache_write_data := UInt(0)
        dcache_read_addr := UInt(0)
        when(dcache_write_en){
            dcache(dcache_write_addr) := dcache_write_data
        }
        val dcache_read_out = dcache(dcache_read_addr)
        val dcache_read_burst = Vec.fill(16){Bits(width=conf.xprlen)}
        val dcache_read_data = Bits(width=conf.xprlen)
         
        val tag = req_addr(31,16)
        val index = req_addr(15,6)
        val word_offset = req_addr(5,2)
        val byte_offset = req_addr(1,0)
        dcache_read_addr := index
        dcache_write_data(DCACHE_BITS-1,DCACHE_BITS-1) := Bits(1,1)
        dcache_write_data(DCACHE_BITS-2,DCACHE_BITS-17) := tag
        val word_data = Bits() 
        val read_data = Bits()
        word_data := Mux1H(UIntToOH(word_offset, width=(burst_len / word_len)),dcache_read_burst)
        read_data := LoadDataGen(word_data >> (byte_offset << 3), req_typ)
    
        for(k <- 0 until 16)
        {
         dcache_read_burst(k) := Bits(0) 
        }
        
        /*
        val test_data = Bits(15,32)
        val write_mask = Bits(0xffffffffL, 32)
        //write access
        when(req_valid && req_fcn === M_XWR){
            //dcache_write_data(UInt(word_offset)*32+31,UInt(word_offset)*32) := test_data
            dcache.write(index, test_data , write_mask << (word_offset<<5))
            printf("!!write access, test_data: %x word_offset: %x \n dcache_read_out: %x\n",test_data,word_offset,dcache_read_out)
            index_reg := index
        }*/

        val s_idle  :: s_load :: Nil = Enum(UInt(),2)
        val state = Reg(init = s_idle)
        io.core_port.resp.valid := Bool(false)
        io.core_port.resp.bits.data := read_data        
        switch(state)
        {
            is(s_idle)
            {
                //printf("idle\n")
                io.core_port.resp.valid := Bool(false)
                
                when ( io.core_port.req.valid )
                {
		    //io.core_port.req.valid := Bool(false)
                    //read access
                    when ( io.core_port.req.bits.fcn === M_XRD )
                    {
		    	//read hit
                        //printf("read\n")
                        io.core_port.resp.valid := Bool(false)
			//printf("tag : %x\n",tag)
                        when(dcache_read_out(DCACHE_BITS-1,DCACHE_BITS-1) === Bits(1,1) 
                             && dcache_read_out(DCACHE_BITS-2,DCACHE_BITS-17) === tag)
                        {
                            {
                        	//printf("read hit\n")
			    	
                                io.core_port.resp.valid := Bool(true)
                                for(k <- 0 until 16)
                                     dcache_read_burst(k) := dcache_read_out(32*k+31,32*k) 

                                word_data := Mux1H(UIntToOH(word_offset, width=(burst_len / word_len)),dcache_read_burst)
                                read_data := LoadDataGen(word_data >> (byte_offset << 3), req_typ)
                                io.core_port.resp.bits.data := read_data
                                state := s_idle
                            }
                        }

                        //cache miss, load memory
                        .otherwise
                        {
                            //printf("read miss\n")
                            io.mem_port.req.valid := Bool(true)
                            io.mem_port.req.bits.addr := io.core_port.req.bits.addr
                            io.mem_port.req.bits.data := io.core_port.req.bits.data
                            io.mem_port.req.bits.fcn := io.core_port.req.bits.fcn
                            io.mem_port.req.bits.typ := io.core_port.req.bits.typ
                            state := s_load
                        }
                    }
                    //write access
                    when ( io.core_port.req.bits.fcn === M_XWR )
                    {
                        //printf("write\n")
                           
                        //write memory
                        io.core_port.resp.valid := Bool(false)
                        io.mem_port.req.valid := Bool(true)
                        io.mem_port.req.bits.addr := io.core_port.req.bits.addr
                        io.mem_port.req.bits.data := io.core_port.req.bits.data
                        io.mem_port.req.bits.fcn := io.core_port.req.bits.fcn
                        io.mem_port.req.bits.typ := io.core_port.req.bits.typ
                        //write hit    
                       	when(dcache_read_out(DCACHE_BITS-1,DCACHE_BITS-1) === Bits(1,1) 
                             && dcache_read_out(DCACHE_BITS-2,DCACHE_BITS-17) === tag)
			{
                            //printf("write hit\n")
                            val wdata = (StoreDataGen(req_data, req_typ)<< UInt(word_offset<<5))
      			    val byte_shift_amt = req_addr(1, 0)
      			    val bit_shift_amt  = Cat(byte_shift_amt, UInt(0,3))
         		    val wmask = (StoreMask(req_typ) << bit_shift_amt)(31,0)
                            val write_mask = wmask << UInt(word_offset<<5)
                            //printf("index= %x, req_data= %x, wdata= %x, wmask=%x \n",index,req_data,wdata,write_mask)
                                  
                            dcache.write(index, wdata, write_mask)
                            when ( io.mem_port.resp.valid )
                            {
                            //    printf("memory response\n")
                                io.core_port.resp.valid := Bool(true)
                                state := s_idle
                            }
			}
			.otherwise{    //write miss
                            //printf("write miss\n")
                            state := s_load
			}
                    }

                }
                val dcache_read_out_2 = dcache(index_reg)
                //printf("index_reg: %x, cache_line: %x\n", index_reg,dcache_read_out_2)
            }
            

            //cache miss, load memory
            is(s_load)
            {
                //printf("load\n")
                io.core_port.resp.valid := Bool(false)
                when ( io.mem_port.resp.valid )
                {   
                    for(k <- 0 until 16) 
                        dcache_write_data(32*k+31,32*k) := burst_data(k)
                    dcache.write(index, dcache_write_data)
                    //printf("abc dcache_write_data: %x\n", dcache_write_data)
                    state := s_idle
                }
            }

        }
}

    type DCache = NoDCache2

    //appropriately mask and sign-extend data for the core                  
    object LoadDataGen                                                      
    {                                                                       
       def apply(data: Bits, typ: Bits) : Bits =                            
       {                                                                    
          val out = Mux(typ === MT_H,  Cat(Fill(16, data(15)),  data(15,0)),
                    Mux(typ === MT_HU, Cat(Fill(16, UInt(0x0)), data(15,0)),
                    Mux(typ === MT_B,  Cat(Fill(24, data(7)),    data(7,0)),
                    Mux(typ === MT_BU, Cat(Fill(24, UInt(0x0)), data(7,0)), 
                                        data(31,0)))))                      
                                                                            
          return out                                                        
       }                                                                    
    }
}

