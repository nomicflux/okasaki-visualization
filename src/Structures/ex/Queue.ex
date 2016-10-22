defmodule Queue do
  import Stack

  # | *Queue
  defstruct front: Stack.empty, back: Stack.empty
  # .end

  # | *top bottom pop eject
  defp rotate(%Queue{front: front, back: back}) do
    %Queue{front: Stack.reverse(back), back: Stack.reverse(front)}
  end
  # .end

  # | *empty
  def empty, do: %Queue{}
  # .end

  # | *top
  def top(%Queue{front: [x | _xs]}), do: x
  def top(queue), do: queue |> rotate |> top
  # .end

  # | *back
  def back(%Queue{back: [x | _xs]}), do: x
  def back(queue), do: queue |> rotate |> back
  # .end

  # | *pop
  def pop(%Queue{front: [_x | xs], back: back}) do
    %Queue{front: xs, back: back}
  end
  def pop(queue), do: queue |> rotate |> pop
  # .end

  # | *eject
  def eject(%Queue{front: front, back: [_y | ys]}) do
    %Queue{front: front, back: ys}
  end
  def eject(queue), do: queue |> rotate |> eject
  # .end

  # | *inject
  def inject(queue, val), do: %Queue{front: Stack.cons(val, queue.front),
                                     back: queue.back}
  # .end

  # | *push
  def push(queue, val), do: %Queue{front: queue.front,
                                   back: Stack.cons(val, queue.back)}
  # .end
end
