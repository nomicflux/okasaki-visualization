defmodule Structures.Stack do
  # | *empty
  def empty, do: []
  # .end

  # | *head
  def head([x | _]), do: x
  # .end

  # | *tail
  def tail([_ | xs]), do: xs
  # .end

  # | *cons
  def cons(x, stack), do: [x | stack]
  # .end

  # | *reverse
  def reverse(stack), do: reverse(stack, [])
  def reverse([], acc), do: acc
  def reverse([x|xs], acc), do: reverse(xs, [x|acc])
  # .end
end
