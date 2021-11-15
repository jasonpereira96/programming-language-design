class Value:
  def __init__(self, s) -> None:
    self.value = s

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


def evaluate(exp):
  if type(exp) == Num:
    return exp.value

  if type(exp) == Add:
    return evaluate(exp.v1) + evaluate(exp.v2)

  if type(exp) == Sub:
    return evaluate(exp.v1) - evaluate(exp.v2)

  if type(exp) == Mul:
    return evaluate(exp.v1) * evaluate(exp.v2)

  if type(exp) == Div:
    return evaluate(exp.v1) / evaluate(exp.v2)

s = Str("123")

print(s.value)
print(type(s) == Str)
e = Add(Num(2), Num(3))
print(evaluate(e))