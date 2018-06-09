#include "thumbsim.hpp"
// These are just the register NUMBERS
#define PC_REG 15  
#define LR_REG 14
#define SP_REG 13

// These are the contents of those registers
#define PC rf[PC_REG]
#define LR rf[LR_REG]
#define SP rf[SP_REG]

Stats stats;
Caches caches(0);

// Done - CPE 315: you'll need to implement a custom sign-extension function
// in addition to the ones given below, specifically for the unconditional
// branch instruction, which has an 11-bit immediate field

unsigned int signExtend11to32 (unsigned int input) {
   int index;

   if (input & (1 << 10)) {
      for (index = 11; index < 32; index++) {
         input |= (1 << index);
      }
   }
   return input;
}

unsigned int signExtend16to32ui(short i) {
  return static_cast<unsigned int>(static_cast<int>(i));
}

unsigned int signExtend8to32ui(char i) {
  return static_cast<unsigned int>(static_cast<int>(i));
}

// This is the global object you'll use to store condition codes N,Z,V,C
// Set these bits appropriately in execute below.
ASPR flags;

// Done - CPE 315: You need to implement a function to set the Negative and Zero
// flags for each instruction that does that. It only needs to take
// one parameter as input, the result of whatever operation is executing
void setNegativeZero(unsigned long a, unsigned int width) {
   flags.N = ((1 << width - 1) & a) != 0 ? 1 : 0;
   flags.Z = a == 0 ? 1 : 0;
}

int countBits (int one) {
   int bits = 0;

   for (int i = 0; i < 8; i++) {
      if ((one >> i ) & 1) {
         bits++;
      }
   }
   return bits;
}

// This function is complete, you should not have to modify it
void setCarryOverflow (int num1, int num2, OFType oftype) {
  switch (oftype) {
    case OF_ADD:
      if (((unsigned long long int)num1 + (unsigned long long int)num2) ==
          ((unsigned int)num1 + (unsigned int)num2)) {
        flags.C = 0;
      }
      else {
        flags.C = 1;
      }
      if (((long long int)num1 + (long long int)num2) ==
          ((int)num1 + (int)num2)) {
        flags.V = 0;
      }
      else {
        flags.V = 1;
      }
      break;
    case OF_SUB:
      if (num1 >= num2) {
        flags.C = 1;
      }
      else if (((unsigned long long int)num1 - (unsigned long long int)num2) ==
          ((unsigned int)num1 - (unsigned int)num2)) {
        flags.C = 0;
      }
      else {
        flags.C = 1;
      }
      if (((num1==0) && (num2==0)) ||
          (((long long int)num1 - (long long int)num2) ==
           ((int)num1 - (int)num2))) {
        flags.V = 0;
      }
      else {
        flags.V = 1;
      }
      break;
    case OF_SHIFT:
      // C flag unaffected for shifts by zero
      if (num2 != 0) {
        if (((unsigned long long int)num1 << (unsigned long long int)num2) ==
            ((unsigned int)num1 << (unsigned int)num2)) {
          flags.C = 0;
        }
        else {
          flags.C = 1;
        }
      }
      // Shift doesn't set overflow
      break;
    default:
      cerr << "Bad OverFlow Type encountered." << __LINE__ << __FILE__ << endl;
      exit(1);
  }
}

// Done - CPE 315: You're given the code for evaluating BEQ, and you'll need to 
// complete the rest of these conditions. See Page 208 of the armv7 manual
static int checkCondition(unsigned short cond) {
  switch(cond) {
    case EQ:
      if (flags.Z == 1) {
        return TRUE;
      }
      break;
    case NE:
      if (flags.Z == 0) {
         return TRUE;
      }
      break;
    case CS:
      if (flags.C == 1) {
         return TRUE;
      }
      break;
    case CC:
      if (flags.C == 0) {
         return TRUE;
      }
      break;
    case MI:
      if (flags.N == 1) {
         return TRUE;
      }
      break;
    case PL:
      if (flags.N == 0) {
         return TRUE;
      }
      break;
    case VS:
      if (flags.V == 1) {
         return TRUE;
      }
      break;
    case VC:
      if (flags.V == 0) {
         return TRUE;
      }
      break;
    case HI:
      if (flags.C == 1 && flags.Z == 0) {
         return TRUE;
      }
      break;
    case LS:
      if (flags.C == 0 || flags.Z == 1) {
         return TRUE;
      }
      break;
    case GE:
      if (flags.N == flags.V) {
         return TRUE;
      }
      break;
    case LT:
      if (flags.N != flags.V) {
         return TRUE;
      }
      break;
    case GT:
      if (flags.Z == 0 && flags.N == flags.V) {
         return TRUE;
      }
      break;
    case LE:
      if (flags.Z == 1 || flags.N != flags.V) {
         return TRUE;
      }
      break;
    case AL:
      return TRUE;
      break;
  }
  return FALSE;
}

void execute() {
  Data16 instr = imem[PC];
  Data16 instr2;
  Data32 temp(0); // Use this for STRB instructions
  Thumb_Types itype;
  // the following counts as a read to PC
  unsigned int pctarget = PC + 2;
  unsigned int addr;
  int i, n, offset;
  unsigned int list, mask;
  int num1, num2, result, BitCount;
  unsigned int bit;
  bool checked;

  /* Convert instruction to correct type */
  /* Types are described in Section A5 of the armv7 manual */
  BL_Type blupper(instr);
  ALU_Type alu(instr);
  SP_Type sp(instr);
  DP_Type dp(instr);
  LD_ST_Type ld_st(instr);
  MISC_Type misc(instr);
  COND_Type cond(instr);
  UNCOND_Type uncond(instr);
  LDM_Type ldm(instr);
  STM_Type stm(instr);
  LDRL_Type ldrl(instr);
  ADD_SP_Type addsp(instr);

  BL_Ops bl_ops;
  ALU_Ops add_ops;
  DP_Ops dp_ops;
  SP_Ops sp_ops;
  LD_ST_Ops ldst_ops;
  MISC_Ops misc_ops;

  // This counts as a write to the PC register
  rf.write(PC_REG, pctarget);
  stats.numRegReads++;
  stats.numRegWrites++;
  stats.instrs++;

  itype = decode(ALL_Types(instr));

  // Done - CPE 315: The bulk of your work is in the following switch statement
  // All instructions will need to have stats and cache access info added
  // as appropriate for that instruction.
  switch(itype) {
    case ALU:
      add_ops = decode(alu);
      switch(add_ops) {
        case ALU_LSLI:
          rf.write(alu.instr.lsli.rd, rf[alu.instr.lsli.rm] << alu.instr.lsli.imm);
          setCarryOverflow(rf[alu.instr.lsli.rm], alu.instr.lsli.imm, OF_SHIFT);
          setNegativeZero(rf[alu.instr.lsli.rd], 32);
          stats.numRegReads++;
          stats.numRegWrites++;
          break;
        case ALU_ADDR:
          // Done - needs stats and flags
          rf.write(alu.instr.addr.rd, rf[alu.instr.addr.rn] + rf[alu.instr.addr.rm]);
          setCarryOverflow(rf[alu.instr.addr.rd], rf[alu.instr.addr.rm], OF_ADD);
          setNegativeZero(rf[alu.instr.addr.rd], 32);
          stats.numRegReads += 2;
          stats.numRegWrites++;
          break;
        case ALU_SUBR:
          rf.write(alu.instr.subr.rd, rf[alu.instr.subr.rn] - rf[alu.instr.subr.rm]);
          setCarryOverflow(rf[alu.instr.subr.rn], rf[alu.instr.subr.rm], OF_SUB);
          setNegativeZero(rf[alu.instr.subr.rd], 32);
          stats.numRegReads += 2;
          stats.numRegWrites++;
          break;
        case ALU_ADD3I:
          // Done - needs stats and flags
          rf.write(alu.instr.add3i.rd, rf[alu.instr.add3i.rn] + alu.instr.add3i.imm);
          setCarryOverflow(rf[alu.instr.add3i.rn], alu.instr.add3i.imm, OF_ADD);
          setNegativeZero(rf[alu.instr.add3i.rd], 32);
          stats.numRegReads++;
          stats.numRegWrites++;
          break;
        case ALU_SUB3I:
          rf.write(alu.instr.sub3i.rd, rf[alu.instr.sub3i.rn] - alu.instr.sub3i.imm);
          setCarryOverflow(rf[alu.instr.sub3i.rn], alu.instr.sub3i.imm, OF_SUB);
          setNegativeZero(rf[alu.instr.sub3i.rd], 32);
          stats.numRegReads++;
          stats.numRegWrites++;
          break;
        case ALU_MOV:
          // Done - needs stats and flags
          rf.write(alu.instr.mov.rdn, alu.instr.mov.imm);
          setNegativeZero(rf[alu.instr.mov.rdn], 32);
          stats.numRegWrites++;
          break;
        case ALU_CMP:
          setCarryOverflow(rf[alu.instr.cmp.rdn], alu.instr.cmp.imm, OF_SUB);
          setNegativeZero(rf[alu.instr.cmp.rdn] - alu.instr.cmp.imm, 32);
          stats.numRegReads++;
          break;
        case ALU_ADD8I:
          // Done - needs stats and flags
          rf.write(alu.instr.add8i.rdn, rf[alu.instr.add8i.rdn] + alu.instr.add8i.imm);
          setCarryOverflow(rf[alu.instr.add8i.rdn], alu.instr.add8i.imm, OF_ADD);
          setNegativeZero(rf[alu.instr.add8i.rdn], 32);
          stats.numRegReads++;
          stats.numRegWrites++;
          break;
        case ALU_SUB8I:
          rf.write(alu.instr.sub8i.rdn, rf[alu.instr.sub8i.rdn] - alu.instr.sub8i.imm);
          setCarryOverflow(rf[alu.instr.sub8i.rdn], alu.instr.sub8i.imm, OF_SUB);
          setNegativeZero(rf[alu.instr.sub8i.rdn], 32);
          stats.numRegReads++;
          stats.numRegWrites++;
          break;
        default:
          cout << "instruction not implemented" << endl;
          exit(1);
          break;
      }
      break;
    case BL: 
      // This instruction is complete, nothing needed here
      bl_ops = decode(blupper);
      if (bl_ops == BL_UPPER) {
        // PC has already been incremented above
        instr2 = imem[PC];
        BL_Type bllower(instr2);
        if (blupper.instr.bl_upper.s) {
          addr = static_cast<unsigned int>(0xff<<24) | 
            ((~(bllower.instr.bl_lower.j1 ^ blupper.instr.bl_upper.s))<<23) |
            ((~(bllower.instr.bl_lower.j2 ^ blupper.instr.bl_upper.s))<<22) |
            ((blupper.instr.bl_upper.imm10)<<12) |
            ((bllower.instr.bl_lower.imm11)<<1);
        }
        else {
          addr = ((blupper.instr.bl_upper.imm10)<<12) |
            ((bllower.instr.bl_lower.imm11)<<1);
        }
        // return address is 4-bytes away from the start of the BL insn
        rf.write(LR_REG, PC + 2);
        // Target address is also computed from that point
        rf.write(PC_REG, PC + 2 + addr);

        stats.numRegReads += 1;
        stats.numRegWrites += 2; 
      }
      else {
        cerr << "Bad BL format." << endl;
        exit(1);
      }
      break;
    case DP:
      dp_ops = decode(dp);
      switch(dp_ops) {
        case DP_CMP:
          // Done - need to implement
          setCarryOverflow(rf[dp.instr.DP_Instr.rdn], rf[dp.instr.DP_Instr.rm], OF_SUB);
          setNegativeZero(rf[dp.instr.DP_Instr.rdn] - rf[dp.instr.DP_Instr.rm], 32);
          stats.numRegReads += 2;
          break;
      }
      break;
    case SPECIAL:
      sp_ops = decode(sp);
      switch(sp_ops) {
        case SP_MOV:
          // ??? - needs stats and flags
          rf.write((sp.instr.mov.d << 3 ) | sp.instr.mov.rd, rf[sp.instr.mov.rm]);
          stats.numRegReads++;
          stats.numRegWrites++;
          break;
        case SP_ADD:
          rf.write((sp.instr.add.d << 3) | sp.instr.add.rd, rf[(sp.instr.add.d << 3) | sp.instr.add.rd] + rf[(sp.instr.add.rm)]);
          stats.numRegReads += 2;
          stats.numRegWrites++;
          break;
        case SP_CMP:
          // Done - need to implement these
          setCarryOverflow(rf[(sp.instr.cmp.d << 3) | sp.instr.cmp.rd], rf[sp.instr.cmp.rm], OF_SUB);
          setNegativeZero(rf[(sp.instr.cmp.d << 3) | sp.instr.cmp.rd] - rf[sp.instr.cmp.rm], 32);
          stats.numRegReads += 2;
          break;
      }
      break;
    case LD_ST:
      // You'll want to use these load and store models
      // to implement ldrb/strb, ldm/stm and push/pop
      ldst_ops = decode(ld_st);
      switch(ldst_ops) {
        case STRI:
          // Done - functionally complete, needs stats
          addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm * 4;
          dmem.write(addr, rf[ld_st.instr.ld_st_imm.rt]);
          caches.access(addr);
          stats.numRegReads += 2;
          stats.numMemWrites++;
          break;
        case LDRI:
          // Done - functionally complete, needs stats
          addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm * 4;
          rf.write(ld_st.instr.ld_st_imm.rt, dmem[addr]);
          caches.access(addr);
          stats.numRegWrites++;
          stats.numRegReads++;
          stats.numMemReads++;
          break;
        case STRR:
          // Done - need to implement
          addr = rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm];
          dmem.write(addr, rf[ld_st.instr.ld_st_reg.rt]);
          caches.access(addr);
          stats.numRegReads += 3;
          stats.numMemWrites++;
          break;
        case LDRR:
          // Done - need to implement
          addr = rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm];
          rf.write(ld_st.instr.ld_st_reg.rt, dmem[addr]);
          caches.access(addr);
          stats.numRegWrites++;
          stats.numRegReads += 2;
          stats.numMemReads++;
          break;
        case STRBI:
          // ??? - need to implement
          addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm;
          temp = dmem[addr];
          //temp = temp & 0xffffff00;
          //dmem.write(addr, temp | ((rf[ld_st.instr.ld_st_reg.rt] & 0x000000ff) << 24));
          temp.set_data_ubyte4(0, rf[ld_st.instr.ld_st_reg.rt] & 0xffffffff);
          dmem.write(addr, temp);
          caches.access(addr);
          stats.numRegReads += 2;
          stats.numMemWrites++;
          break;
        case LDRBI:
          // ??? - need to implement
          addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm;
          rf.write(ld_st.instr.ld_st_reg.rt, dmem[addr].data_ubyte4(0));
          caches.access(addr);
          stats.numRegReads++;
          stats.numRegWrites++;
          stats.numMemReads++;

          break;
        case STRBR:
          // ??? - need to implement
          addr = rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm];
          temp = dmem[addr];
          //temp = temp & 0xffffff00;
          //dmem.write(addr, temp | ((rf[ld_st.instr.ld_st_reg.rt] & 0x000000ff) << 24));
          temp.set_data_ubyte4(0, rf[ld_st.instr.ld_st_reg.rt] & 0xffffffff);
          dmem.write(addr, temp);
          caches.access(addr);
          stats.numRegReads += 3;
          stats.numMemWrites++;

          break;
        case LDRBR:
          // ??? - need to implement
          addr = rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm];
          rf.write(ld_st.instr.ld_st_reg.rt, dmem[addr].data_ubyte4(0));
          caches.access(addr);
          stats.numRegReads += 2;
          stats.numRegWrites++;
          stats.numMemReads++;
          break;
      }
      break;
    case MISC:
      misc_ops = decode(misc);
      switch(misc_ops) {
        case MISC_PUSH:
          // ??? - need to implement

         addr = SP;
         if (misc.instr.push.m) {
            addr -= 4;
            caches.access(addr);
            dmem.write(addr, LR);
            stats.numMemWrites++;
            stats.numRegReads++;
         }
         for (i = 7; i >= 0; i--) {
            if ((1 << i) & misc.instr.push.reg_list) {
               addr -= 4;
               caches.access(addr);
               dmem.write(addr, rf[i]);
               stats.numMemWrites++;
               stats.numRegReads++;
            }
         }
         rf.write(SP_REG, addr);
         stats.numRegReads++;
         stats.numRegWrites++;
          
          break;
        case MISC_POP:
          // ??? - need to implement
          
         addr = SP;
         for (i = 7; i >= 0; i--) {
            if ((1 << i) & misc.instr.pop.reg_list) {
               caches.access(addr);
               rf.write(i, dmem[addr]);
               addr += 4;
               stats.numMemReads++;
               stats.numRegWrites++;
            }
         }
         if (misc.instr.pop.m) {
            caches.access(addr);
            rf.write(PC_REG, dmem[addr]);
            addr += 4;
            stats.numMemReads++;
            stats.numRegWrites++;
         }
         rf.write(SP_REG, addr);
         stats.numRegReads++;
         stats.numRegWrites++;

          break;
        case MISC_SUB:
          // Done - functionally complete, needs stats
          rf.write(SP_REG, SP - (misc.instr.sub.imm*4));
          stats.numRegWrites++;
          stats.numRegReads++;
          break;
        case MISC_ADD:
          // Done - functionally complete, needs stats
          rf.write(SP_REG, SP + (misc.instr.add.imm*4));
          stats.numRegWrites++;
          stats.numRegReads++;
          break;
      }
      break;
    case COND:
      decode(cond);
      checked = checkCondition(cond.instr.b.cond);
      // Once you've completed the checkCondition function,
      // this should work for all your conditional branches.
      // Done - needs stats
      
      if (checked) {
         if (PC < PC + 2 * signExtend8to32ui(cond.instr.b.imm) + 2) {
            stats.numForwardBranchesTaken++;
         }
         else {
            stats.numBackwardBranchesTaken++;
         }
         rf.write(PC_REG, PC + 2 * signExtend8to32ui(cond.instr.b.imm) + 2);
         stats.numRegWrites++;
         stats.numRegReads++;
      }
      else {
         if (PC < PC + 2 * signExtend8to32ui(cond.instr.b.imm) + 2) {
            stats.numForwardBranchesNotTaken++;
         }
         else {
            stats.numBackwardBranchesNotTaken++;
         }
      }
      
      break;
    case UNCOND:
      // Done - Essentially the same as the conditional branches, but with no
      // condition check, and an 11-bit immediate field
      decode(uncond);
      rf.write(PC_REG, PC + 2 * signExtend11to32(uncond.instr.b.imm) + 2);
      stats.numRegWrites++;
      stats.numRegReads++;
      break;
    case LDM:
      decode(ldm);
      // ??? - need to implement <--------------------------------
      BitCount = countBits(ldm.instr.ldm.reg_list);
      addr = rf[ldm.instr.ldm.rn];
      for (i = 0; i < 8; i++) {
         if ((ldm.instr.ldm.reg_list >> i) & 1) {
            rf.write(i, dmem[addr]);
            caches.access(addr);
            addr += 4;
         }
      }
      rf.write(ldm.instr.ldm.rn, rf[ldm.instr.ldm.rn] + (BitCount * 4));
      stats.numMemReads += BitCount;
      stats.numRegWrites += BitCount + 1;
      stats.numRegReads++;

      break;
    case STM:
      decode(stm);
      // ??? - need to implement <--------------------------------
      BitCount = countBits(stm.instr.stm.reg_list);
      addr = rf[stm.instr.stm.rn];
      for (i = 0; i < 8; i++) {
         if ((stm.instr.stm.reg_list >> i) & 1) {
            dmem.write(addr, rf[i]);
            caches.access(addr);
            addr += 4;
            stats.numRegReads++;
         }
      }
      rf.write(stm.instr.stm.rn, rf[stm.instr.stm.rn] + BitCount * 4);
      stats.numMemWrites += BitCount;
      stats.numRegWrites++;
      stats.numRegReads++;

      break;
    case LDRL:
      // This instruction is complete, nothing needed
      decode(ldrl);
      // Need to check for alignment by 4
      if (PC & 2) {
        addr = PC + 2 + (ldrl.instr.ldrl.imm)*4;
      }
      else {
        addr = PC + (ldrl.instr.ldrl.imm)*4;
      }
      // Requires two consecutive imem locations pieced together
      temp = imem[addr] | (imem[addr+2]<<16);  // temp is a Data32
      rf.write(ldrl.instr.ldrl.rt, temp);

      // One write for updated reg
      stats.numRegWrites++;
      // One read of the PC
      stats.numRegReads++;
      // One mem read, even though it's imem, and there's two of them
      stats.numMemReads++;
      break;
    case ADD_SP:
      // Done - needs stats
      decode(addsp);
      rf.write(addsp.instr.add.rd, SP + (addsp.instr.add.imm*4));
      stats.numRegWrites++;
      stats.numRegReads++;
      break;
    default:
      cout << "[ERROR] Unknown Instruction to be executed" << endl;
      exit(1);
      break;
  }
}
