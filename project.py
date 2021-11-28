___ret___ = '___ret___'

class Value:
  """The class Value represents a value in a program. Values considered for now are
  numbers, booleans, strings and arrays. All these are types are implemented in Python
  as subclasses of Value
  """
  def __init__(self, v) -> None:
    self.value = v

class Map(Value):
  """
  Represents an Map in the program. 
  """
  def __init__(self, key_datatype=None, value_datatype=None) -> None:
    self.key_datatype = key_datatype
    self.value_datatype = value_datatype
    self.map = {}

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
class LessThanEq(BinaryExp):
  pass
class NotEq(BinaryExp):
  pass
class Mod(BinaryExp):
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

class FunctionDecl(Command):
  def __init__(self, function) -> None:
    self.function = function

class Call:
  def __init__(self, function_name, args) -> None:
    self.function_name = function_name
    self.args = args

class Ret(Command):
  def __init__(self, ret_value) -> None:
    self.ret_value = ret_value

class Function(Value):
  def __init__(self, name, body) -> None:
    self.name = name
    self.body = body

def lookup(key, stack):
  for state in reversed(stack):
    if key in state:
      return state[key]

  raise Exception("{} not defined".format(key))

def update_state(key, value, stack):
  stack[-1][key] = value

def evaluate_args(args, stack):
  evaluated_args = {}
  for k in dict.keys(args):
    evaluated_args[k] = evaluate(args[k], stack)
  return evaluated_args

# the evaluate function which is intented to evalute an expression takes the state as well
# because it needs read-only access to the state for the subscript operation (Eg: arr[2])
def evaluate(exp, stack): 
  assert stack is not None and len(stack) > 0

  if type(exp) == Num:
    return exp.value

  if type(exp) == Bool:
    return exp.value

  if type(exp) == Str:
    return exp.value

  if type(exp) == Var:
    # return state[exp.value]
    return lookup(exp.value, stack)


  if type(exp) == Array:
    return exp.values

  if type(exp) == Add:
    return evaluate(exp.v1, stack) + evaluate(exp.v2, stack) # Add works for both string concatenation and numeric addition

  if type(exp) == Sub:
    return evaluate(exp.v1, stack) - evaluate(exp.v2, stack)

  if type(exp) == Mul:
    return evaluate(exp.v1, stack) * evaluate(exp.v2, stack)

  if type(exp) == Div:
    return evaluate(exp.v1, stack) / evaluate(exp.v2, stack)
  
  if type(exp) == And:
    return evaluate(exp.v1, stack) and evaluate(exp.v2, stack)

  if type(exp) == Or:
    return evaluate(exp.v1, stack) or evaluate(exp.v2, stack)

  if type(exp) == Eq:
    return evaluate(exp.v1, stack) == evaluate(exp.v2, stack)

  if type(exp) == NotEq:
    return evaluate(exp.v1, stack) != evaluate(exp.v2, stack)

  if type(exp) == LessThan:
    return evaluate(exp.v1, stack) < evaluate(exp.v2, stack)

  if type(exp) == LessThanEq:
    return evaluate(exp.v1, stack) <= evaluate(exp.v2, stack)
  
  if type(exp) == Mod:
    return evaluate(exp.v1, stack) % evaluate(exp.v2, stack)


  if type(exp) == GetField:
    index = evaluate(exp.exp, stack)
    # assert type(index) == int 
    array = lookup(exp.ident, stack=stack)
    # assert index < len(array)
    return array[index]

  if type(exp) == Map:
    return exp.map

  if type(exp) == Function:
    return {
      'name': exp.name,
      'body': exp.body,
      'is_function': True
    }

  if type(exp) == Call:
    return execute(exp, stack)
  

def execute(command, stack):
  assert stack is not None and len(stack) > 0

  if type(command) == Assignment:
    assignment_statement = command
    value = evaluate(assignment_statement.exp, stack)
    assignee = assignment_statement.assignee
    # state[assignee] = value
    update_state(assignee, value, stack)

  elif type(command) == Sequence:
    execute(command.c1, stack)
    execute(command.c2, stack)

  elif type(command) == IfElse:
    is_true = evaluate(command.condition, stack)
    if is_true:
      execute(command.c1, stack)
    else:
      execute(command.c2, stack)

  elif type(command) == BlockStatement:
    for c in command.command_list:
      execute(c, stack)

  elif type(command) == IfElifElse:
    N = len(command.conditions)
    for i in range(N):
      if evaluate(command.conditions[i], stack):
        execute(command.commands[i], stack)
        return
    
    execute(command.commands[-1], stack)
    # return state

  elif type(command) == While:
    c = evaluate(command.condition, stack)
    if c == True:
      execute(command.block_statement, stack)
      execute(command, stack)
  

  elif type(command) == SetField:
    k = evaluate(command.exp, stack)
    v = evaluate(command.exp_to_set, stack)
    # array = state[command.assignee]
    array = lookup(command.assignee, stack)
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

    execute(initialization, stack)
    s = While(condition=condition, block_statement=BlockStatement(command_list=[body, update]))
    execute(s, stack)

  elif type(command) == FunctionDecl:
    # state[command.function.name] = command.function.body
    update_state(command.function.name, command.function.body, stack)
    
  # return state
  elif type(command) == Call:
    function_bs = lookup(command.function_name, stack)#.function_body
    args = evaluate_args(command.args, stack)
    stack.append(args)
    execute(function_bs, stack) # adding an activation frame to the stack
    stack.pop()
    # now the stack should have a ___ret___
    ret_value = stack[-1][___ret___]
    del stack[-1][___ret___]
    return ret_value

  elif type(command) == Ret:
    if len(stack) == 1:
      raise "return statement outside function"
  
    ret_value = evaluate(command.ret_value, stack)
    stack[-2][___ret___] = ret_value



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
stack = [init_state]
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

s13 = Assignment("m", Map())
s14 = SetField("m", Num(2), Num(20))
s15 = Assignment("kl", GetField("m", Num(2)))


s16 = FunctionDecl(Function("mff", BlockStatement(command_list=[Ret(Add(Var("a"), Var("b")))])))
s17 = Assignment("total1", Call("mff", {'a': Num(100), 'b': Num(500)}))

f1 = IfElifElse(conditions=[Eq(Var("n"), Num(0)), Eq(Var("n"), Num(1))], 
  commands=[Ret(Var("n")), Ret(Var("n")), 
  BlockStatement([
    Ret(Add(Call("fib", {'n': Sub(Var('n'), Num(1))}), Call("fib", {'n': Sub(Var('n'), Num(2))})))
  ])]
)
s18 = FunctionDecl(Function("fib", f1))

s19 = Assignment("xxx", Call("fib", {'n': Num(14)}))


statements = [s1,s2,s3,s4, s5, s6, s7, s8, s9, s11, s12, s13, s14, s15, s16, s17, s18, s19]
# statements = [s18, s19]

# fib sequence
# [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

# Program to find sum of the first N numbers
'''
total := 0
for i := 0; i <= 10; i++ {
  total := total + i
}
'''
program1 = [
  Assignment("total", Num(0)),
  For(
    Assignment("i", Num(1)),
    LessThanEq(Var("i"), Num(10)),
    Assignment("i", Add(Var("i"), Num(1))),
    BlockStatement([Assignment("total", Add(Var("total"), Var("i")))])
  )
]
# Program to find the sum of an array
'''
var a [5]int
for j := 1; j < 5; j++ {
  a[j] = j * 2
}
var total := 0
for j := 1; j < 5; j++ {
  total = total + a[j]
}
'''
program2 = [
  Assignment("a", Array(Num, 5)),
  For(Assignment("j", Num(1)), LessThan(Var("j"), Num(5)), Assignment("j", Add(Var('j'), Num(1))), block_statement=BlockStatement([
    SetField("a", Var("j"), Mul(Var('j'), Num(2)))
  ])),
  Assignment("total", Num(0)),
  For(Assignment("j", Num(1)), LessThan(Var("j"), Num(5)), Assignment("j", Add(Var('j'), Num(1))), block_statement=BlockStatement([
    Assignment("total", Add(Var("total"), GetField("a", Var("j")))),
  ]))
]
# Program to find GCD of 2 numbers
'''
func gcd(a, b int) int {
	for b != 0 {
		t := b
		b = a % b
		a = t
	}
	return a
}

func main() {
  var g int = gcd(130, 13)
}
'''
program3 = [
  FunctionDecl(Function("gcd", BlockStatement([
    For(BlockStatement([]), NotEq(Var("b"), Num(0)), BlockStatement([]), BlockStatement([
      Assignment("t", Var("b")),
      Assignment("b", Mod(Var("a"), Var("b"))),
      Assignment("a", Var("t"))
    ])),
    Ret(Var("a"))
  ]))),
  Assignment("g", Call("gcd", {"a": Num(130), "b": Num(13)}))
]
'''
func fib(n int) int {
	if n == 0 {
    return 0
  } else if n == 1 {
    return 1
  } else {
    return fib(n-1) + fib(n-2)
  }
}
func main() {
    fib(14)
}
'''

f1 = IfElifElse(conditions=[Eq(Var("n"), Num(0)), Eq(Var("n"), Num(1))], 
  commands=[Ret(Var("n")), Ret(Var("n")), 
  BlockStatement([
    Ret(Add(Call("fib", {'n': Sub(Var('n'), Num(1))}), Call("fib", {'n': Sub(Var('n'), Num(2))})))
  ])]
)
f2 = FunctionDecl(Function("fib", f1))
f3 = Assignment("f", Call("fib", {'n': Num(14)}))

program4 = [f2, f3]

programs = [
  program1,
  program2,
  program3,
  program4
]
for program in programs:
  init_state = {}
  for statement in program:
    execute(statement, stack=[init_state])
  print(init_state)
