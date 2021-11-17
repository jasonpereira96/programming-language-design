class Value:
  """The class Value represents a value in a program. Values considered for now are
  numbers, booleans, strings and arrays. All these are types are implemented in Python
  as subclasses of Value
  """
  def __init__(self, v) -> None:
    self.value = v

class Array(Value):
  """
  Represents an Array in the program. Default length is 1. This is not exactly according to the
  Go constructor for an array but will improve this later on. Go requires that the programmer declare the
  length of the array while declaring it. That is reflected in the length argument of the constructor.
  """
  def __init__(self, length=1) -> None:
    self.values = [0] * length

class Str(Value):
  pass

class Num(Value):
  pass

class Bool(Value):
  pass

class Exp:
  def __init__(self) -> None:
    pass

class BinaryExp(Exp):
  """A BinaryExp represents an expression which has 2 operands and one operator in between like 
  a+b, a-b, a*b
  """
  def __init__(self, v1, v2) -> None:
      super(Exp, self).__init__()
      self.v1 = v1
      self.v2 = v2

class Add(BinaryExp):
  pass

class Sub(BinaryExp):
  pass

class Mul(BinaryExp):
  pass

class Div(BinaryExp):
  pass

class And(BinaryExp):
  pass

class Or(BinaryExp):
  pass

class GetField(Exp):
  """This represents the subscript operation for an array in Go. Need to change this name to something better.
  For example: a[1], a[0], a[2+3]
  """
  def __init__(self, ident, exp) -> None:
    self.ident = ident
    self.exp = exp

class Command:
  pass


class BlockStatement(Command):
  """A Block statement represents a an entire block of statements in Go. Block statements are commonly used in
  for, while loops and if else conditions. A BlockStatement has a list of commands inside.

  For example:
  for j := 0; j < 3; j++ { <--------- BlockStatement
    fmt.Println("Value:")
    fmt.Println(a[j])
  }
  
  """
  def __init__(self, command_list=[]) -> None:
    super(Command, self).__init__()
    self.command_list = command_list

class Assignment(Command):
  def __init__(self, assignee, exp) -> None:
    super(Command, self).__init__()
    self.assignee = assignee
    self.exp = exp

class Sequence(Command):
  def __init__(self, c1, c2) -> None:
    super(Command, self).__init__()
    self.c1 = c1
    self.c2 = c2

class IfElse(Command):
  def __init__(self, condition, c1, c2) -> None:
    super(Command, self).__init__()
    self.condition = condition
    self.c1 = c1
    self.c2 = c2

class IfElifElse(Command):
  def __init__(self, conditions, commands) -> None:
    super(Command, self).__init__()

    N = len(conditions)
    assert len(commands) == N + 1

    self.conditions = conditions
    self.commands = commands




def evaluate(exp, state={}):
  if type(exp) == Num:
    return exp.value

  if type(exp) == Bool:
    return exp.value

  if type(exp) == Str:
    return exp.value

  if type(exp) == Array:
    return exp.values

  if type(exp) == Add:
    return evaluate(exp.v1) + evaluate(exp.v2) # Add works for both string concatenation and numeric addition

  if type(exp) == Sub:
    return evaluate(exp.v1) - evaluate(exp.v2)

  if type(exp) == Mul:
    return evaluate(exp.v1) * evaluate(exp.v2)

  if type(exp) == Div:
    return evaluate(exp.v1) / evaluate(exp.v2)
  
  if type(exp) == And:
    return evaluate(exp.v1) and evaluate(exp.v2)

  if type(exp) == Or:
    return evaluate(exp.v1) or evaluate(exp.v2)

  if type(exp) == GetField:
    get_field = exp
    index = evaluate(get_field.exp)
    assert type(index) == int
    return state[get_field.ident][index]
  

def execute(command, state):

  if type(command) == Assignment:
    assignment_statement = command
    value = evaluate(assignment_statement.exp)
    assignee = assignment_statement.assignee
    state[assignee] = value

  elif type(command) == Sequence:
    execute(command.c1, state)
    execute(command.c2, state)

  elif type(command) == IfElse:
    is_true = evaluate(command.condition)
    if is_true:
      execute(command.c1, state)
    else:
      execute(command.c2, state)

  elif type(command) == BlockStatement:
    for c in command.command_list:
      execute(c, state)

  elif type(command) == IfElifElse:
    N = len(command.conditions)
    for i in range(N):
      if evaluate(command.conditions[i]):
        execute(command.commands[i], state)
        return state
    
    execute(command.commands[-1], state)
    return state
    
  return state

s = Str("123")

print(s.value)
print(type(s) == Str)
e = Sub(Add(Num(2), Num(3)), Num(1))
as1 = Assignment("x", e)
as2 = Assignment("y", e)
seq = Sequence(as1, as2)

print(evaluate(e))
print(evaluate(Add(Str("x"), Str("y"))))

init_state = {}
execute(as1, init_state)
execute(seq, init_state)

cds = [
  Assignment("a", Num(1)),
  Assignment("b", Num(2)),
  Assignment("c", Num(3)),
]
bs = BlockStatement(cds)

execute(bs, init_state)
execute(IfElifElse([Bool(True), Bool(True)], [Assignment("a", Num(8)),Assignment("a", Num(9)),Assignment("a", Num(10))]), init_state)
print(init_state)
execute(Assignment("arr", Array(3)), init_state)
evaluate(GetField("arr", Num(0)), init_state)
print(init_state)