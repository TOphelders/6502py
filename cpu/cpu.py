import instructions

class CPU:
  class Memory:
    #The 6502 has a 16-bit adress bus, indicating max usable memory size of 65,536 bytes (64 KB)
    def __init__(self, size=0x10000):
      self._size = min(size, 0x10000)
      self._ram = [0] * self._size
      
    def __getitem__(self, address):
      if address < self._size:
        return self._ram[address]
      else:
        raise Exception('Memory read out of bounds')

    def __setitem__(self, address, value):
      if 0 <= address < self._size:
        self._ram[address] = value
      else:
        raise Exception('Memory write out of bounds')

    def read(self, address):
      return self[address]

    def write(self, address, value):
      self[address] = value

  class Register:
    #The 6502 contains 3 8-bit general purpose registers, a status register,
    #an 8-bit stack pointer and a 16-bit program counter
    def __init__(self):
      self._value = 0

    def read(self):
      return self._value

    def write(self, value):
      self._value = value

    def set_bit(self, bit, state):
      if state:
        self._value |= (1 << bit)
      else:
        self._value &= (1 << bit)
    
    def read_bit(self, bit):
      mask = 1 << bit
      return (self._value & mask) >> bit
  
  class Instruction:
    def __init__(self, cpu, instruction, admode, cycles):
      self._cpu = cpu
      self._instruction = instruction
      self._admode = admode
      self._cycles = cycles

    #Pass in the binary data following op-code for our instruction
    def __call__(self, binary):
      args = []
      for i in range(admode.size, -1, -1):
        args.append((binary >> 8 * i) & 0xff)

      #Return the value to pass into the argument after it has operated on the given arguments
      val = _admode.do(args)

      self._instruction(self._cpu, val)

  def __init__(self):
    self.memory = CPU.Memory()
    self.registers = {
      'a': CPU.Register(),
      'x': CPU.Register(),
      'y': CPU.Register(),
      'status': CPU.Register(),
      'sp': CPU.Register(),
      'pc': CPU.Register()
    }
    #Status bit 5 is always set to 1
    self.registers['status'].set_bit(5, 1)

  def set_status(self, sbit, state):
    {
      'C': lambda s: self.registers['status'].set_bit(0, s), #Carry
      'Z': lambda s: self.registers['status'].set_bit(1, s), #Zero
      'I': lambda s: self.registers['status'].set_bit(2, s), #Interrupt
      'D': lambda s: self.registers['status'].set_bit(3, s), #Decimal
      'B': lambda s: self.registers['status'].set_bit(4, s), #Software Interrupt (BRK)
      'V': lambda s: self.registers['status'].set_bit(6, s), #Overflow
      'S': lambda s: self.registers['status'].set_bit(7, s)  #Sign
    }[sbit](state)
  
  def get_status(self, sbit):
    return {
      'C': self.registers['status'].read_bit(0), #Carry
      'Z': self.registers['status'].read_bit(1), #Zero
      'I': self.registers['status'].read_bit(2), #Interrupt
      'D': self.registers['status'].read_bit(3), #Decimal
      'B': self.registers['status'].read_bit(4), #Software Interrupt (BRK)
      'V': self.registers['status'].read_bit(6), #Overflow
      'S': self.registers['status'].read_bit(7)  #Sign
    }[sbit]

if __name__ == '__main__':
  test = CPU()
  test.memory[0x10f3] = 0x12
  print(test.memory[0x10f3])
  test.registers['a'].write(0x00)
  print(test.registers['a'].read())
  test.registers['status'].write(0)
  test.set_status('C', 1)
  print(test.get_status('C'))
