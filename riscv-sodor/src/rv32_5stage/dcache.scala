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
        
        val burst_data     = io.mem_port.resp.bits.burst_data
        

	//DCACHE configuration
        val DCACHE_ENTRIES = 1024
        val DCACHE_ENTRIES_BIT = 10
        val DCACHE_TAG_BIT = conf.xprlen-DCACHE_ENTRIES_BIT-burst_len_bit
        val DCACHE_BITS = 1+DCACHE_TAG_BIT+burst_len*8
        
        val dcache = Mem(Bits(width=DCACHE_BITS), DCACHE_ENTRIES)
        val dcache_write_data = UInt(width=DCACHE_BITS)
        val dcache_read_burst = Vec.fill(16){Bits(width=conf.xprlen)}
        val dcache_read_data = Bits(width=conf.xprlen)
         
        val tag = req_addr(conf.xprlen-1,conf.xprlen-DCACHE_TAG_BIT)
        val index = req_addr(conf.xprlen-DCACHE_TAG_BIT-1,conf.xprlen-DCACHE_TAG_BIT-DCACHE_ENTRIES_BIT)
        val word_offset = req_addr(word_len + word_len_bit -1 , word_len_bit)
        val byte_offset = req_addr(word_len_bit-1,0)
        val dcache_read_out = dcache(index)
        val word_data = Bits() 
        val read_data = Bits()
        
	//FSM Information
        val s_idle  :: s_load :: Nil = Enum(UInt(),2)
        val state = Reg(init = s_idle)
	
	//Init
        for(k <- 0 until 16)
            dcache_read_burst(k) := Bits(0) 
        
	// Wiring
        io.mem_port.req.valid := Bool(false)
        io.mem_port.req.bits.addr := io.core_port.req.bits.addr
        io.mem_port.req.bits.data := io.core_port.req.bits.data
        io.mem_port.req.bits.fcn := io.core_port.req.bits.fcn
        io.mem_port.req.bits.typ := io.core_port.req.bits.typ
        io.core_port.resp.valid := Bool(false)
        io.core_port.resp.bits.data := read_data        
        	    
	//set valid = 1 tag bit 
	dcache_write_data := UInt(0)
	dcache_write_data(DCACHE_BITS-1,DCACHE_BITS-1) := Bits(1,1)
        dcache_write_data(DCACHE_BITS-2,DCACHE_BITS-1-DCACHE_TAG_BIT) := tag

	//Load data
        word_data := Mux1H(UIntToOH(word_offset, width=(burst_len / word_len)),dcache_read_burst)
        read_data := LoadDataGen(word_data >> (byte_offset << 3), req_typ)
	
	//store data
        val wdata = (StoreDataGen(req_data, req_typ)<< UInt(word_offset<<5))
        val byte_shift_amt = req_addr(1, 0)
        val bit_shift_amt  = Cat(byte_shift_amt, UInt(0,3))
        val wmask = (StoreMask(req_typ) << bit_shift_amt)(31,0)
        val write_mask = wmask << UInt(word_offset<<5)
        
	val csr = Module(new CSRFile())
	
	//Control CACHE signal
        switch(state) //idle or load
        {
            is(s_idle)
            {
                when ( io.core_port.req.valid )
                {
                    //read access
                    when ( io.core_port.req.bits.fcn === M_XRD )
                    {
		    	//read hit
                        when(dcache_read_out(DCACHE_BITS-1,DCACHE_BITS-1) === Bits(1,1) 
                             && dcache_read_out(DCACHE_BITS-2,DCACHE_BITS-1-DCACHE_TAG_BIT) === tag)
                        {
                            {
                        	printf("read hit\n")
                                io.core_port.resp.valid := Bool(true)
                                for(k <- 0 until 16)
                                     dcache_read_burst(k) := dcache_read_out(32*k+31,32*k) 
                                io.core_port.resp.bits.data := read_data
                                state := s_idle
                            }
                        }
                        //read miss
                        .otherwise
                        {
                            printf("read miss\n")
                            io.mem_port.req.valid := Bool(true)
                            state := s_load
                        }
                    }
                    //write access
                    when ( io.core_port.req.bits.fcn === M_XWR )
                    {
                        //write through    always write memory
                        io.mem_port.req.valid := Bool(true)

                        //write hit    
                       	when(dcache_read_out(DCACHE_BITS-1,DCACHE_BITS-1) === Bits(1,1) 
                             && dcache_read_out(DCACHE_BITS-2,DCACHE_BITS-1-DCACHE_TAG_BIT) === tag)
			{
                            printf("write hit\n")
                            dcache.write(index, wdata, write_mask)
                            when ( io.mem_port.resp.valid )
                            {
                                io.core_port.resp.valid := Bool(true)
                                state := s_idle
                            }
			}
			.otherwise{    //write miss
                            printf("write miss\n")
                            state := s_load
			}
                    }

                }
            }
            //read/write miss , load memory
            is(s_load)
            {
                when ( io.mem_port.resp.valid )
                {   
                    for(k <- 0 until 16) 
                        dcache_write_data(32*k+31,32*k) := burst_data(k)
                    dcache.write(index, dcache_write_data)
		    when(io.core_port.req.bits.fcn === M_XWR){
                    	io.core_port.resp.valid := Bool(true)
		    }
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

