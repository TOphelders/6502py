#All instructions will be represented by an instance of a class, therefore all instructions take two possible arguments

#===========================
#Load and Store Instructions
#===========================
def _load(cpu, val, reg):
  cpu.registers[reg].write(val)

  cpu.set_status('S', val & 0x80)
  cpu.set_status('Z', not val)

def _store(cpu, addr, reg):
  val = cpu.registers[reg].read()
  cpu.memory[addr] = val

def LDA(cpu, val):
  _load(cpu, val, 'a')

def LDX(cpu, val):
  _load(cpu, val, 'x')

def LDY(cpu, val):
  _load(cpu, val, 'y')

def STA(cpu, addr):
  _store(cpu, addr, 'a')

def STX(cpu, addr):
  _store(cpu, addr, 'x')

def STY(cpu, addr):
  _store(cpu, addr, 'y')

#=======================
#Arithmetic instructions
#=======================
def ADC(cpu, val):
  init = cpu.registers['a'].read()
  sum = init + val + cpu.get_status('C')

  #Convert our binary val to Binary Coded Decimal format if flag is set
  if cpu.get_status('D'):
    if (init & 0xf) + (val & 0xf) + cpu.get_status('C') > 9:
      sum += 6
    if sum > 0x99:
      sum += 96
    cpu.set_status('C', sum > 0x99)
  else:
    cpu.set_status('C', sum > 0xff)

  cpu.registers['a'].write(sum & 0xff)

  cpu.set_status('Z', not (sum & 0xff))
  cpu.set_status('S', sum & 0x80)
  cpu.set_status('V', ((init ^ sum) & (val ^ sum) & 0x80))

# Uncertain of behaviour when subtracting larger BCD number from smaller, need to update
def SBC(cpu, val):
  init = cpu.registers['a'].read()
  #We will use straight addtion of two's complement vals to subtract
  comp = (~val + 1) & 0xff
  borrow = not cpu.get_status('C')
  bcomp = (~borrow + 1) & 0xff
  diff = init + comp + bcomp

  if cpu.get_status('D'):
    if (init & 0xf) < (val & 0xf) + borrow:
      diff += (~6 + 1) & 0xff
    if (diff & 0xff) > 0x99:
      diff += (~60 + 1) & 0xff
    cpu.set_status('C', diff > 0x99)
  else:
    cpu.set_status('C', diff > 0xff)

  cpu.registers['a'].write(diff & 0xff)

  cpu.set_status('Z', not (diff & 0xff))
  cpu.set_status('S', diff & 0x80)
  cpu.set_status('V', ((init ^ diff) & (val ^ diff) & 0x80))

#====================================
#Increment and Decrement Instructions
#====================================
def _inc_mem(cpu, addr, step):
  val = (cpu.memory.read(addr) + step) & 0xff
  cpu.memory.write(addr, val)

  cpu.set_status('Z', not val)
  cpu.set_status('S', val & 0x80)

def _inc_reg(cpu, reg, step):
  val = (cpu.registers[reg] + step) & 0xff
  cpu.registers[reg].write(val)
  
  cpu.set_status('Z', not val)
  cpu.set_status('S', val & 0x80)

def INC(cpu, addr):
  _inc_mem(cpu, addr, 1)

def INX(cpu, *args):
  _inc_reg(cpu, 'x', 1)

def INY(cpu, *args):
  _inc_reg(cpu, 'y', 1)

def DEC(cpu, addr):
  _inc_mem(cpu, addr, 0xff)

def DEX(cpu, *args):
  _inc_reg(cpu, 'x', 0xff)

def DEY(cpu, *args):
  _inc_reg(cpu, 'y', 0xff)

#====================
#Logical instructions
#====================
def _bit_op(cpu, addr, op):
  acc = cpu.registers['a'].read()
  mem = cpu.memory[addr]
  res = 0

  if op == '&':
    res = acc & mem
  elif op == '|':
    res = acc | mem
  elif op == '^':
    res = acc ^ mem

  cpu.registers['a'].write(res)

  cpu.set_status('Z', not res)
  cpu.set_status('S', res & 0x80)

def AND(cpu, addr):
  _bit_op(cpu, addr, '&')

def ORA(cpu, addr):
  _bit_op(cpu, addr, '|')

def EOR(cpu, addr):
  _bit_op(cpu, addr, '^')
  
#===================================
#Jump, Branch, Compare and Test Bits
#===================================
def _branch(cpu, disp, bit, isset):
  if cpu.get_status(bit) == isset:
    pc = cpu.registers['pc'].read()
    branch = disp | 0xff00 if disp & 0x80 else disp

    addr = (pc + branch) & 0xffff
    cpu.registers['pc'].write(addr)

def _compare(cpu, val, reg):
  reg = cpu.registers[reg].read()
  comp = (~val + 1) & 0xff
  diff = reg + comp
  
  cpu.set_status('C', diff > 0xff)
  cpu.set_status('Z', not (diff & 0xff))
  cpu.set_status('S', diff & 0x80)

def JMP(cpu, addr):
  cpu.registers['pc'].write(addr)

def BCC(cpu, disp):
  _branch(cpu, disp, 'C', False)

def BCS(cpu, disp):
  _branch(cpu, disp, 'C', True)

def BEQ(cpu, disp):
  _branch(cpu, disp, 'Z', True)

def BNE(cpu, disp):
  _branch(cpu, disp, 'Z', False)

def BMI(cpu, disp):
  _branch(cpu, disp, 'S', True)

def BPL(cpu, disp):
  _branch(cpu, disp, 'S', False)

def BVS(cpu, disp):
  _branch(cpu, disp, 'V', True)

def BVC(cpu, disp):
  _branch(cpu, disp, 'V', False)

def CMP(cpu, val):
  _compare(cpu, val, 'a')

def CPX(cpu, val):
  _compare(cpu, val, 'x')

def CPY(cpu, val):
  _compare(cpu, val, 'y')

def BIT(cpu, addr):
  reg = cpu.registers['a'].read()
  mem = cpu.memory[addr]
  
  cpu.set_status('S', mem & 0x80)
  cpu.set_status('V', mem & 0x40)
  cpu.set_status('Z', mem & reg)

#=============================
#Shift and Rotate Instructions
#=============================
def _shift(cpu, addr, right):
  init = 0
  new = 0
  if addr:
    init = cpu.memory[addr[0]]
    new = init >> 1 if right else init << 1
    cpu.memory[addr[0]] = new & 0xff
  else:
    init = cpu.registers['a'].read()
    new = init >> 1 if right else init << 1
    cpu.registers['a'].write(new & 0xff)

  cpu.set_status('C', init & 1 if right else new > 0xff)
  cpu.set_status('Z', not (new & 0xff))
  cpu.set_status('S', new & 0x80)

def ASL(cpu, *addr):
  _shift(cpu, addr, False)
  
def LSR(cpu, *addr):
  _shift(cpu, addr, True)

def ROL(cpu, *addr):
  val = 0
  if addr:
    val = cpu.memory[addr[0]]
    val = val << 1 | cpu.get_status('C')
    cpu.memory[addr[0]] = val & 0xff
  else:
    val = cpu.registers['a'].read()
    val = val << 1 | cpu.get_status('C')
    cpu.registers['a'].write(val & 0xff)
  
  cpu.set_status('C', val > 0xff)
  cpu.set_status('Z', not (val & 0xff))
  cpu.set_status('S', val & 0x80)

def ROR(cpu, *addr):
  init = 0
  new = 0
  if addr:
    init = cpu.memory[addr[0]]
    new = init >> 1 | cpu.get_status('C') << 7
    cpu.memory[addr[0]] = new
  else:
    init = cpu.registers['a'].read()
    new = init >> 1 | cpu.get_status('C') << 7
    cpu.registers['a'].write(new)
  
  cpu.set_status('C', init & 1)
  cpu.set_status('Z', not new)
  cpu.set_status('S', new & 0x80)

#=====================
#Transfer Instructions
#=====================
def _transfer(cpu, start, target):
  val = cpu.registers[start].read()
  cpu.registers[target].write(val)

  cpu.set_status('S', val & 0x80)
  cpu.set_status('Z', not val)

def TAX(cpu, *args):
  _tansfer(cpu, 'a', 'x')

def TAY(cpu, *args):
  _tansfer(cpu, 'a', 'y')

def TXA(cpu, *args):
  _tansfer(cpu, 'x', 'a')

def TYA(cpu, *args):
  _tansfer(cpu, 'y', 'a')
  
#==================
#Stack Instructions
#==================
def _push(cpu, reg):
  val = cpu.registers[reg].read()
  sp = cpu.registers['sp'].read()

  cpu.memory[0x100 + sp] = val 
  sp = (sp + 0xff) & 0xff
  cpu.registers['sp'].write(sp)

def _pull(cpu, reg):
  sp = cpu.registers['sp'].read()
  sp = (sp + 1) & 0xff
  val = cpu.memory[0x100 + sp]
  
  cpu.registers[reg].write(val)
  cpu.registers['sp'].write(sp)

def TSX(cpu, *args):
  sp = cpu.registers['sp'].read()
  cpu.registers['x'].write(sp) 

def TXS(cpu, *args):
  x = cpu.registers['x'].read()
  cpu.registers['sp'].write(x)

def PHA(cpu, *args):
  _push(cpu, 'a')

def PHP(cpu, *args):
  _push(cpu, 'status')

def PLA(cpu, *args):
  _pull(cpu, 'a')

def PLP(cpu, *args):
  _pull(cpu, 'status')

#=======================
#Subroutine Instructions
#=======================
def JSR(cpu, addr):
  pc_low = (cpu.registers['pc'].read() + 0xffff) & 0xff
  pc_high = ((cpu.registers['pc'].read() + 0xffff) & 0xff00) >> 8
  sp = cpu.registers['sp'].read()
  
  cpu.memory[0x100 + sp] = pc_high
  sp = (sp + 0xff) & 0xff
  cpu.memory[0x100 + sp] = pc_low
  sp = (sp + 0xff) & 0xff
  cpu.registers['sp'].write(sp)
  
  cpu.registers['pc'].write(addr)

def RTS(cpu, *args):
  sp = cpu.registers['sp'].read()
  sp = (sp + 1) & 0xff
  pc_low = cpu.memory[0x100 + sp]
  sp = (sp + 1) & 0xff
  pc_high = cpu.memory[0x100 + sp]
  cpu.registers['sp'].write(sp)

  pc = pc_low + (pc_high << 8)
  pc = (pc + 1) & 0xffff
  cpu.registers['pc'].write(pc)

def RTI(cpu, *args):
  sp = cpu.registers['sp'].read()
  sp = (sp + 1) & 0xff
  status = cpu.memory[0x100 + sp]
  cpu.registers['status'].write(status)

  sp = (sp + 1) & 0xff
  pc_low = cpu.memory[0x100 + sp]

  sp = (sp + 1) & 0xff
  pc_high = cpu.memory[0x100 + sp]
  cpu.registers['sp'].write(sp)

  pc = pc_low + (pc_high << 8)
  cpu.registers['pc'].write(pc)

#==================================
#Set and Reset (Clear) Instructions
#==================================
def CLC(cpu, *args):
  cpu.set_status('C', 0)

def SEC(cpu, *args):
  cpu.set_status('C', 1)

def CLI(cpu, *args):
  cpu.set_status('I', 0)

def SEI(cpu, *args):
  cpu.set_status('I', 1)

def CLV(cpu, *args):
  cpu.set_status('V', 0)

def CLD(cpu, *args):
  cpu.set_status('D', 0)

def SED(cpu, *args):
  cpu.set_status('D', 1)

#==================
#Other Instructions
#==================
def NOP(cpu, *args):
  pass #2 Cycles

def BRK(cpu, *args):
  cpu.set_status('B', 1)

  pc_low = (cpu.registers['pc'].read() + 1) & 0xff
  pc_high = ((cpu.registers['pc'].read() + 1) & 0xff00) >> 8
  status = cpu.registers['status'].read()
  sp = cpu.registers['sp'].read()

  cpu.memory[0x100 + sp] = pc_high
  sp = (sp + 0xff) & 0xff
  cpu.memory[0x100 + sp] = pc_low
  sp = (sp + 0xff) & 0xff
  cpu.memory[0x100 + sp] = status
  sp = (sp + 0xff) & 0xff
  cpu.registers['sp'].write(sp)

  target_low = cpu.memory[0xfffe]
  target_high = cpu.memory[0xffff]
  pc = target_low + (target_high << 8)

  cpu.registers['pc'].write(pc)
