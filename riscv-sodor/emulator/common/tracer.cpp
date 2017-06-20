#include "tracer.h"


// emulator.cpp passes in a pointer to the Instruction Register 
// found in the simulated processor.
Tracer_t::Tracer_t(dat_t<32>* _inst_ptr, dat_t<1>* _stats_reg, FILE* log, dat_t<1>* _full_stall_ptr)
//Tracer_t::Tracer_t(dat_t<32>* _inst_ptr, FILE* log)
{
   inst_ptr = _inst_ptr;
   stats_reg = _stats_reg;
   is_freeze = _full_stall_ptr;
   logfile  = log;
   paused   = 1;
}

// Initializes and turns the tracer on. 
// HOWEVER the tracer will NOT collect stats until the StatsEnable register
// (co-processor register cr10) has been set in the processor.
void Tracer_t::start()
{
   paused = 0;
   
   trace_data.cycles = 0;
   trace_data.inst_count = 0;
                  
   trace_data.nop_count   = 0;
   trace_data.bubble_count= 0;
   trace_data.ldst_count  = 0;
   trace_data.arith_count = 0;
   trace_data.br_count    = 0;  
   trace_data.misc_count  = 0;  
   trace_data.load_count  = 0;
   trace_data.store_count = 0;
   trace_data.dcache_miss_cycles = 0;
   trace_data.read_miss_count = 0;
   trace_data.write_miss_count = 0;

   /* XXX Step 2: INITIALIZE YOUR COUNTERS HERE */
}


// pull bits from a bit-array
// getBits(inst, 6,0) returns inst[6:0]
int getBits(uint32_t data_bits, int hi_bit, int lo_bit)
{
   int sz = hi_bit - lo_bit + 1;
   int offset  = lo_bit;
   int sz_mask = 0xFFFFFFFF >> (32-sz);
   return ((data_bits >> offset) & sz_mask);
}

// Input: boolean increment instruction count?
// For regular RISC-V pipelines, this will always be set to true on every clock
// cycle in the calling function.
//
// For other more exotic pipelines (like the micro-coded pipeline), inst_count
// is only incremented on instruction fetch.
//
// NOTE: we still do not increment inst_count for machine-generated NOP/bubbles,
//   which are represented by (XOR, x0, x0, x0).
//
void Tracer_t::tick(bool increment_inst_count)
{
   // only collect stats if the tracer is not paused AND co-processor 
   // register cr10 is enabled.
   trace_data.cycles++;

   // translate from the Chisel/Emulator data wrapper to uint32_t
   uint32_t inst = inst_ptr->lo_word();

   // Consult the riscv-spec.pdf for help (in particular the Major Opcode Map).
   uint32_t opcode = getBits(inst,6,0); 
   uint32_t opc_hi = getBits(inst,6,5); 
   uint32_t opc_lo = getBits(inst,4,2); 

   if (is_freeze == nullptr || *is_freeze == 0x0) {
      // don't increment on machine-generated bubbles
      if (increment_inst_count && inst != 0x4033)
         trace_data.inst_count++;
      
      if (inst == 0x13) 
         trace_data.nop_count++;
      else if (inst == 0x4033) 
         trace_data.bubble_count++;
      else if (opcode == 0x37) //lui
         trace_data.misc_count++;
      else if (opcode == 0x63) 
         trace_data.br_count++;
      else if (opcode == 0x03 || opcode == 0x23) {
         trace_data.ldst_count++;
         if (opcode == 0x03)
            trace_data.load_count++;
         else
            trace_data.store_count++;
      }
      else if (opc_lo == 0x6 || opc_lo == 0x4) 
         trace_data.arith_count++;
      else
         trace_data.misc_count++;
   }
   else {
      trace_data.dcache_miss_cycles++;
      if (last_is_freeze == 0x0) {
         if (last_opcode == 0x03)
            trace_data.read_miss_count++;
         else if (last_opcode == 0x23)
            trace_data.write_miss_count++;
      }
   }

    last_is_freeze = *is_freeze;
    last_opcode = opcode;

}

void Tracer_t::stop()
{
   paused = 1;
}

void Tracer_t::print()
{
   fprintf(logfile, "\n");
   fprintf(logfile, "#----------- Tracer Data -----------\n");
   
   if (trace_data.cycles == 0)
      fprintf(logfile, "\n#     No stats collected: Tracer_t::start() never called.\n\n");
   else
      fprintf(logfile, "#\n");
   
   fprintf(logfile, "#      CPI   : %2.2f\n",  ((float) trace_data.cycles) / trace_data.inst_count);
   fprintf(logfile, "#      IPC   : %2.2f\n",  ((float) trace_data.inst_count) / trace_data.cycles);
   fprintf(logfile, "#      cycles: %lu\n",  trace_data.cycles);
   fprintf(logfile, "#\n");
   fprintf(logfile, "#      Bubbles     : %2.3f %%\n",  100.0f * ((float) trace_data.bubble_count)/ trace_data.cycles);
   fprintf(logfile, "#      Nop instr   : %2.3f %%\n",  100.0f * ((float) trace_data.nop_count  ) / trace_data.cycles);
   fprintf(logfile, "#      Arith instr : %2.3f %%\n",  100.0f * ((float) trace_data.arith_count) / trace_data.cycles);
   fprintf(logfile, "#      Ld/St instr : %2.3f %%\n",  100.0f * ((float) trace_data.ldst_count ) / trace_data.cycles);
   fprintf(logfile, "#      branch instr: %2.3f %%\n",  100.0f * ((float) trace_data.br_count   ) / trace_data.cycles);
   fprintf(logfile, "#      misc instr  : %2.3f %%\n",  100.0f * ((float) trace_data.misc_count ) / trace_data.cycles);
   fprintf(logfile, "#      D-cache miss stall: %2.3f %%\n",  100.0f * ((float) trace_data.dcache_miss_cycles ) / trace_data.cycles);
   fprintf(logfile, "#\n");
   fprintf(logfile, "#      Read miss rate: %2.3f %%\n",  100.0f * ((float) trace_data.read_miss_count ) / trace_data.load_count);
   fprintf(logfile, "#      Write miss rate: %2.3f %%\n",  100.0f * ((float) trace_data.write_miss_count ) / trace_data.store_count);
   
   /* XXX Step 4. PRINT YOUR COUNTERS HERE */
   
   
   fprintf(logfile, "#-----------------------------------\n");
   fprintf(logfile, "\n");


}
