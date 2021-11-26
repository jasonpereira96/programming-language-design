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
  def __init__(self, datatype, length) -> None:
    assert length is not None
    assert datatype is not None

    self.values = [0] * length
    self.datatype = datatype

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
class Eq(BinaryExp):
  pass
class LessThan(BinaryExp):
  pass

class Var(Exp):
  def __init__(self, ident) -> None:
    self.value = ident
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

class SetField(Command):
  def __init__(self, assignee, exp, exp_to_set) -> None:
    # assignee[exp] = exp_to_set
    super(Command, self).__init__()
    self.assignee = assignee
    self.exp = exp
    self.exp_to_set = exp_to_set

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

class While(Command):
  def __init__(self, condition, block_statement) -> None:
    super(Command, self).__init__()
    self.condition = condition
    self.block_statement = block_statement

'''
for j := 7; j <= 9; j++ {
  fmt.Println(j)
}
'''
class For(Command):
  def __init__(self, initialization, condition, update, block_statement) -> None:
    super(Command, self).__init__()

    # assert type(block_statement) == BlockStatement
    # assert issubclass(Exp,type(condition))

    self.initialization = initialization
    self.condition = condition
    self.update = update
    self.block_statement = block_statement

# the evaluate function which is intented to evalute an expression takes the state as well
# because it needs read-only access to the state for the subscript operation (Eg: arr[2])
def evaluate(exp, state={}): 
  if type(exp) == Num:
    return exp.value

  if type(exp) == Bool:
    return exp.value

  if type(exp) == Str:
    return exp.value

  if type(exp) == Var:
    return state[exp.value]

  if type(exp) == Array:
    return exp.values

  if type(exp) == Add:
    return evaluate(exp.v1, state) + evaluate(exp.v2, state) # Add works for both string concatenation and numeric addition

  if type(exp) == Sub:
    return evaluate(exp.v1, state) - evaluate(exp.v2, state)

  if type(exp) == Mul:
    return evaluate(exp.v1, state) * evaluate(exp.v2, state)

  if type(exp) == Div:
    return evaluate(exp.v1, state) / evaluate(exp.v2, state)
  
  if type(exp) == And:
    return evaluate(exp.v1, state) and evaluate(exp.v2, state)

  if type(exp) == Or:
    return evaluate(exp.v1, state) or evaluate(exp.v2, state)

  if type(exp) == Eq:
    return evaluate(exp.v1, state) == evaluate(exp.v2, state)

  if type(exp) == LessThan:
    return evaluate(exp.v1, state) < evaluate(exp.v2, state)

  if type(exp) == GetField:
    index = evaluate(exp.exp, state)
    assert type(index) == int 
    array = state[exp.ident]
    assert index < len(array)
    print("returning")  
    return array[index]
  

def execute(command, state):

  if type(command) == Assignment:
    assignment_statement = command
    value = evaluate(assignment_statement.exp, state)
    assignee = assignment_statement.assignee
    state[assignee] = value

  elif type(command) == Sequence:
    execute(command.c1, state)
    execute(command.c2, state)

  elif type(command) == IfElse:
    is_true = evaluate(command.condition, state)
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
      if evaluate(command.conditions[i], state):
        execute(command.commands[i], state)
        return state
    
    execute(command.commands[-1], state)
    return state

  elif type(command) == While:
    c = evaluate(command.condition, state)
    if c == True:
      execute(command.block_statement, state)
      execute(command, state)
  

  elif type(command) == SetField:
    k = evaluate(command.exp, state)
    v = evaluate(command.exp_to_set, state)
    array = state[command.assignee] 
    array[k] = v

  elif type(command) == For:
    # self.initialization = initialization
    # self.condition = condition
    # self.update = update
    # self.block_statement = block_statement

    initialization = command.initialization
    condition = command.condition
    update = command.update
    body = command.block_statement

    execute(initialization, state)
    s = While(condition=condition, block_statement=BlockStatement(command_list=[body, update]))
    execute(s, state)
    
  return state


"""
The following Go program being executed:
func main() {

  var a int = 0
	var arr [5]int
	var result = 0

	if a == 0 {
		result = 5
	} else {
		result = arr[0]
	}
}
"""

init_state = {} # state is being stored as a Python dictionary
s1 = Assignment("a", Num(0))
s2 = Assignment("arr", Array(Num, 5))
s3 = Assignment("result", Num(0))

s4 = IfElse(Eq ( Var("a"), Num(0)), 
  Assignment("result", Num(5)), 
  Assignment("result", GetField("arr", Num(0)))
)

s5 = SetField("arr", Num(2), Num(5))
s6 = SetField("arr", Num(3), Num(2))
s7 = SetField("arr", Num(4), Num(1))

s8 = Assignment("a", Num(0))

s9 = Assignment("a", GetField("arr", Num(0))) # a = a + 0

bs = BlockStatement([
  Assignment("a", Add(Var("a"), Num(100)))
])

s11 = While(LessThan(Var("a"), Num(500)), bs)

s12 = For(initialization=Assignment("counter", Num(0)), 
  condition=LessThan(Var("counter"), Num(10)), 
  update=Assignment("counter", Add(Var("counter"), Num(1))), block_statement=BlockStatement([]))

statements = [s1,s2,s3,s4, s5, s6, s7, s8, s9, s11, s12]

for statement in statements:
  execute(statement, init_state)

print(init_state)
