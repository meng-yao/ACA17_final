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

        // Rename io.core.port
        val req_valid      = io.core_port.req.valid
        val req_addr       = io.core_port.req.bits.addr
        val req_data       = io.core_port.req.bits.data
        val req_fcn        = io.core_port.req.bits.fcn
        val req_typ        = io.core_port.req.bits.typ

	val req_addr_reg       = Reg(Bits())
        val req_data_reg       = Reg(Bits())                                                          
        val req_fcn_reg        = Reg(Bits())
        val req_typ_reg        = Reg(Bits())
	val isFull = Reg(init=Bool(false))
	val isBusy = Reg(init=Bool(false))

        // Generate read data from memory
        val burst_data     = io.mem_port.resp.bits.burst_data

	//DCACHE configuration
        val DCACHE_ENTRIES = 1024
        val DCACHE_ENTRIES_BIT = log2Up(DCACHE_ENTRIES)
        val DCACHE_TAG_BIT = conf.xprlen-DCACHE_ENTRIES_BIT-burst_len_bit
        val DCACHE_BITS = 1+DCACHE_TAG_BIT+burst_len*8
        
        val tag = req_addr(conf.xprlen-1,conf.xprlen-DCACHE_TAG_BIT)
        val index = req_addr(conf.xprlen-DCACHE_TAG_BIT-1,conf.xprlen-DCACHE_TAG_BIT-DCACHE_ENTRIES_BIT)
        val word_offset = req_addr(burst_len_bit -1 , 2)
        val byte_offset = req_addr(1,0)
        
	val dcache = Mem(Bits(width=DCACHE_BITS), DCACHE_ENTRIES, seqRead = false)
        val dcache_write_data = UInt(width=DCACHE_BITS)
        val dcache_read_out = dcache(index)
        val dcache_read_burst = Vec.fill(16){Bits(width=conf.xprlen)}
	
        val word_data = Bits() 
        val read_data = Bits()
        

	//Load data
        for(k <- 0 until 16)
            dcache_read_burst(k) := dcache_read_out(32*k+31,32*k)  
        word_data := Mux1H(UIntToOH(word_offset, width=(burst_len / word_len)),dcache_read_burst)
        read_data := LoadDataGen(word_data >> (byte_offset << 3), req_typ)
        
	
	//set valid = 1 tag bit 
	dcache_write_data := UInt(0)
	dcache_write_data(DCACHE_BITS-1,DCACHE_BITS-1) := Bits(1,1)
        dcache_write_data(DCACHE_BITS-2,DCACHE_BITS-1-DCACHE_TAG_BIT) := tag
	for(k <- 0 until 16) 
	    dcache_write_data(32*k+31,32*k) := burst_data(k)

	//store data
        val wdata = (StoreDataGen(req_data, req_typ)<< UInt(word_offset<<5))
        val bit_shift_amt  = Cat(byte_offset, UInt(0,3))
        val wmask = (StoreMask(req_typ) << bit_shift_amt)(31,0)
        val write_mask = wmask << UInt(word_offset<<5)
	
	// Wiring
        io.mem_port.req.valid := Bool(false)
        io.mem_port.req.bits.addr := io.core_port.req.bits.addr
        io.mem_port.req.bits.data := io.core_port.req.bits.data
        io.mem_port.req.bits.fcn := io.core_port.req.bits.fcn
        io.mem_port.req.bits.typ := io.core_port.req.bits.typ
        io.core_port.resp.valid := Bool(false)
        io.core_port.resp.bits.data := read_data       

	when(isFull && !isBusy){
	    io.mem_port.req.valid := Bool(true)
	    io.mem_port.req.bits.addr := req_addr_reg
            io.mem_port.req.bits.data := req_data_reg
            io.mem_port.req.bits.fcn := req_fcn_reg
            io.mem_port.req.bits.typ := req_typ_reg
	    isBusy := Bool(true)
            isFull := Bool(false)
	}
	when(io.mem_port.resp.valid && isBusy){
	    isBusy := Bool(false)
	}

	printf("is_full: %x, is_busy: %x\n", isFull, isBusy)
        printf("req_addr_reg: %x\nreq_data_reg: %x \nreq_fcn_reg : %x \nreq_typ_reg : %x \n"
            , req_addr_reg
            , req_data_reg
            , req_fcn_reg 
            , req_typ_reg)
        printf("mem.resp.valid: %x\n", io.mem_port.resp.valid)	

        // Define state machine
        val s_idle :: s_load :: Nil = Enum(UInt(),2)
        val state = Reg(init = s_idle)
        switch(state)
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
			    when(!isFull && !isBusy)
			    {
                                io.core_port.resp.valid := Bool(true)
		                state := s_idle
                            }
                        }
                        //read miss
                        .otherwise
                        {
			    when(!isBusy && !isFull){
                                io.mem_port.req.valid := Bool(true)
				isBusy := Bool(true)
                                state := s_load
			    }
                        }
                    }
                    //write access
		    when ( io.core_port.req.bits.fcn === M_XWR )
                    {
		
                        //write hit    
                       	when(dcache_read_out(DCACHE_BITS-1,DCACHE_BITS-1) === Bits(1,1) 
                             && dcache_read_out(DCACHE_BITS-2,DCACHE_BITS-1-DCACHE_TAG_BIT) === tag)
			{
			    when(!isFull){
      			       req_addr_reg      := io.core_port.req.bits.addr
      			       req_data_reg      := io.core_port.req.bits.data  
      			       req_fcn_reg       := io.core_port.req.bits.fcn  
      			       req_typ_reg       := io.core_port.req.bits.typ
                               dcache.write(index, wdata, write_mask)
                               io.core_port.resp.valid := Bool(true)
			       isFull := Bool(true)
 			    }
			}
			.otherwise{    //write miss
			    when(!isBusy && !isFull){
                                io.mem_port.req.valid := Bool(true)
			        isBusy := Bool(true)
                                state := s_load
			    }
			}
                    }
                }
            }
            
            //Read miss, load memory
            is(s_load)
            {
                when ( io.mem_port.resp.valid )
                {   
                    dcache.write(index, dcache_write_data)
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

