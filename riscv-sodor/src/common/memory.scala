//**************************************************************************
// Scratchpad Memory (asynchronous)
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Jun 12
//
// Provides a variable number of ports to the core, and one port to the HTIF
// (host-target interface).
//
// Assumes that if the port is ready, it will be performed immediately.
// For now, don't detect write collisions.
//
// Optionally uses synchronous read (default is async). For example, a 1-stage
// processor can only ever work using asynchronous memory!

package Common
{

import Chisel._
import Node._

import Constants._

trait MemoryOpConstants 
{
   val MT_X  = Bits(0, 3)
   val MT_B  = Bits(1, 3)
   val MT_H  = Bits(2, 3)
   val MT_W  = Bits(3, 3)
   val MT_D  = Bits(4, 3)
   val MT_BU = Bits(5, 3)
   val MT_HU = Bits(6, 3)
   val MT_WU = Bits(7, 3)

   val M_X   = Bits("b00", 2)
   val M_XRD = Bits("b00", 2) // regular and burst read
   val M_XWR = Bits("b10", 2) // regular write
   val M_XWRBURST = Bits("b11", 2) // burst write
}

// from the pov of the datapath
class MemPortIo(data_width: Int)(implicit conf: SodorConfiguration) extends Bundle 
{
   val req    = Decoupled(new MemReq(data_width))
   val resp   = (new ValidIO(new MemResp(data_width))).flip
  override def clone = { new MemPortIo(data_width).asInstanceOf[this.type] }
}

class MemReq(data_width: Int)(implicit conf: SodorConfiguration) extends Bundle
{
   val addr = UInt(width = conf.xprlen)
   val data = Bits(width = data_width)
   val burst_data = Vec.fill(16) { Bits(width = data_width) }
   val fcn  = Bits(width = M_X.getWidth)  // memory function code
   val typ  = Bits(width = MT_X.getWidth) // memory type
  override def clone = { new MemReq(data_width).asInstanceOf[this.type] }
}

class MemResp(data_width: Int) extends Bundle
{
   val data = Bits(width = data_width)
   val burst_data = Vec.fill(16) { Bits(width = data_width) }
  override def clone = { new MemResp(data_width).asInstanceOf[this.type] }
}

// NOTE: the default is enormous (and may crash your computer), but is bound by
// what the fesvr expects the smallest memory size to be.  A proper fix would
// be to modify the fesvr to expect smaller sizes.
class ScratchPadMemory(num_core_ports: Int, num_bytes: Int = (1 << 21), 
   seq_read: Boolean = false // This parameter is deprecated
   )(implicit conf: SodorConfiguration) extends Module
{
   val io = new Bundle
   {
      val core_ports = Vec.fill(num_core_ports) { (new MemPortIo(data_width = conf.xprlen)).flip }
      val htif_port = (new MemPortIo(data_width = 64)).flip
   }


   // HTIF min packet size is 8 bytes 
   // but 32b core will access in 4 byte chunks
   // thus we will bank the scratchpad
   val num_bytes_per_line = 4 + 4
   val num_banks = 2
   val num_lines = num_bytes / num_bytes_per_line
   val count = 10
   println("\n    Sodor Tile: creating Asynchronous Scratchpad Memory of size " + num_lines*num_bytes_per_line/1024 + " kB\n")
   val data_bank0 = Mem(Bits(width = 8*4), num_lines, seqRead = false)
   val data_bank1 = Mem(Bits(width = 8*4), num_lines, seqRead = false)
   val burst_len = 64

   // constants
   val idx_lsb = log2Up(num_bytes_per_line) 
   val bank_bit = log2Up(num_bytes_per_line/num_banks)
   val burst_len_bit = log2Up(burst_len)
   
   val s_idle :: s_load :: s_valid :: Nil = Enum(UInt(),3)
   val state = Reg(init = s_idle)
   

   for (i <- 0 until num_core_ports)
   {
      val req_valid      = Bool()
      val req_addr       = Bits()
      val req_data       = Bits()
      val req_fcn        = Bits()
      val req_typ        = Bits()
      val req_burst_data = Vec.fill(16) { Bits() }
      val req_valid_reg      = Reg(Bool())
      val req_addr_reg       = Reg(Bits())
      val req_data_reg       = Reg(Bits())
      val req_fcn_reg        = Reg(Bits())
      val req_typ_reg        = Reg(Bits())
      val req_burst_data_reg = Vec.fill(16) { Reg(Bits()) }
      val counter = Counter(50 - 1) // 50 cycles for a memory operation

      if (i == 0) {
         req_valid      := io.core_ports(i).req.valid
         req_addr       := io.core_ports(i).req.bits.addr
         req_data       := io.core_ports(i).req.bits.data
         req_fcn        := io.core_ports(i).req.bits.fcn
         req_typ        := io.core_ports(i).req.bits.typ
         req_burst_data := io.core_ports(i).req.bits.burst_data
      }
      else if (i == 1) {
         req_valid      := req_valid_reg 
         req_addr       := req_addr_reg
         req_data       := req_data_reg
         req_fcn        := req_fcn_reg
         req_typ        := req_typ_reg   
         req_burst_data := req_burst_data_reg
      }

      io.core_ports(i).req.ready := Bool(true)
      if(i == 1) {
         io.core_ports(i).resp.valid := Bool(false)
         switch(state){
         is(s_idle)
         {
            io.core_ports(i).resp.valid:=Bool(false)
            io.core_ports(i).req.ready:=Bool(true)
            req_valid_reg      := io.core_ports(i).req.valid
            req_addr_reg       := io.core_ports(i).req.bits.addr
            req_data_reg       := io.core_ports(i).req.bits.data
            req_fcn_reg        := io.core_ports(i).req.bits.fcn
            req_typ_reg        := io.core_ports(i).req.bits.typ
            for (k <- 0 until 16) {
               req_burst_data_reg(k) := io.core_ports(i).req.bits.burst_data(k)
            }
            when(io.core_ports(i).req.valid)
            {
               state:=s_load
            }
         }
         is(s_load)
         {
            io.core_ports(i).resp.valid:=Bool(false)
            io.core_ports(i).req.ready:=Bool(false)
            req_valid_reg      := req_valid_reg      
            req_addr_reg       := req_addr_reg       
            req_data_reg       := req_data_reg       
            req_fcn_reg        := req_fcn_reg        
            req_typ_reg        := req_typ_reg        
            for (k <- 0 until 16) {
               req_burst_data_reg(k) := req_burst_data_reg(k)
            }
            when(counter.inc())
            {
               state:=s_valid
            }
         }
         is(s_valid){
            io.core_ports(i).resp.valid:=Bool(true)
            io.core_ports(i).req.ready:=Bool(false)
            req_valid_reg      := Bool(false) 
            req_addr_reg       := Bits(0)
            req_data_reg       := Bits(0)
            req_fcn_reg        := M_X
            req_typ_reg        := MT_X
            for (k <- 0 until 16) {
               req_burst_data_reg(k) := Bits(0)
            }
            state:=s_idle
         }

         }
      }
      else { 
         io.core_ports(i).resp.valid := io.core_ports(i).req.valid
         io.core_ports(i).req.ready := Bool(true)
      }
      
      val byte_shift_amt = req_addr(1, 0)
      val bit_shift_amt  = Cat(byte_shift_amt, UInt(0,3))

      // read access
      val data_idx = req_addr >> UInt(idx_lsb)
      val bank_idx = req_addr(bank_bit)
      val read_data_out = Bits()
      val rdata_out = Bits()


      read_data_out := Mux(bank_idx, data_bank1(data_idx), data_bank0(data_idx))
      rdata_out     := LoadDataGen((read_data_out >> bit_shift_amt), req_typ)

      if (i == 1) {
         when (state === s_valid) {
            io.core_ports(i).resp.bits.data := rdata_out
         }
         .otherwise {
            io.core_ports(i).resp.bits.data := Bits(0)
         }
      }
      else {
         io.core_ports(i).resp.bits.data := rdata_out
      }

      // write access
      when (state === s_load && counter.value === UInt(48) && req_valid && req_fcn === M_XWR)
      {
         // move the wdata into position on the sub-line
         val wdata = StoreDataGen(req_data, req_typ) 
         val wmask = (StoreMask(req_typ) << bit_shift_amt)(31,0)

         when (bank_idx)
         {
            data_bank1.write(data_idx, wdata, wmask)
         }
         .otherwise
         {
            data_bank0.write(data_idx, wdata, wmask)
         }
      }

      // Burst read
      io.core_ports(i).resp.bits.burst_data := Bits(0)
      if (i == 1) {
         when (state === s_valid) {
            for (j <- 0 until (burst_len / num_bytes_per_line)) {
               val data_idx_burst = Cat(req_addr >> burst_len_bit, Bits(j, width = (burst_len_bit - idx_lsb)))
               io.core_ports(i).resp.bits.burst_data(j * 2) := data_bank0(data_idx_burst)            
               io.core_ports(i).resp.bits.burst_data(j * 2 + 1) := data_bank1(data_idx_burst)            
            }
         }
      }

      // Burst write
      if (i == 1) {
         when (state === s_load && counter.value === UInt(48) && req_valid && req_fcn === M_XWRBURST) {
            for (j <- 0 until (burst_len / num_bytes_per_line)) {
               val data_idx_burst = Cat(req_addr >> burst_len_bit, Bits(j, width = (burst_len_bit - idx_lsb)))
               data_bank0(data_idx_burst) := req_burst_data(j * 2)
               data_bank1(data_idx_burst) := req_burst_data(j * 2 + 1)
            }
         }
      }
   }  


   // HTIF -------
   
   io.htif_port.req.ready := Bool(true) // for now, no back pressure
   // synchronous read
   val htif_idx = Reg(UInt())
   htif_idx := io.htif_port.req.bits.addr >> UInt(idx_lsb)
   
   io.htif_port.resp.valid     := Reg(next=io.htif_port.req.valid && io.htif_port.req.bits.fcn === M_XRD)
   io.htif_port.resp.bits.data := Cat(data_bank1(htif_idx), data_bank0(htif_idx))

   when (io.htif_port.req.valid && io.htif_port.req.bits.fcn === M_XWR)
   {
      data_bank0(htif_idx) := io.htif_port.req.bits.data(31,0)
      data_bank1(htif_idx) := io.htif_port.req.bits.data(63,32)
   }

}



object StoreDataGen
{
   def apply(din: Bits, typ: Bits): UInt =
   {
      val word = (typ === MT_W) || (typ === MT_WU)
      val half = (typ === MT_H) || (typ === MT_HU)
      val byte_ = (typ === MT_B) || (typ === MT_BU)

      val dout =  Mux(byte_, Fill(4, din( 7,0)),
                  Mux(half,  Fill(2, din(15,0)),
                             din(31,0)))
      return dout
   }
}


object StoreMask
{
   def apply(sel: UInt): UInt = 
   {
      val mask = Mux(sel === MT_H || sel === MT_HU, Bits(0xffff, 32),
                 Mux(sel === MT_B || sel === MT_BU, Bits(0xff, 32),
                                                    Bits(0xffffffffL, 32)))

      return mask
   }
}

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
