"""Need to solve this in imperative-programming first because I don't get what I'm doing wrong"""

def get_mem(layout: str) -> list[int]:
    """Exact same logic as my parser"""
    res = []
    for i, c in enumerate(layout):
      if i & 1:
        res.extend([None for _ in range(int(c))])
      else:
        res.extend([i >> 1] * int(c))
    return res

def compact_mem(memory: list[int|None]) -> list[int|None]:
  """
  Input is same as haskell, but here i can freely mutate and while loop...
  """
  
  left, right = 0, len(memory) - 1
  
  while left < right:
    if memory[left] is None:
      memory[left] = memory[right]
      right -= 1
    
    left += 1
    while right and memory[right] is None:
      right -= 1
  
  return memory[:left]

def main():
  layouts = ["12345"] #["2333133121414131402", "12345"]  
  memories = [get_mem(l) for l in layouts]
  print(memories)
  compact_memories = [compact_mem(m) for m in memories]
  print(compact_memories)

if __name__ == "__main__":
  main()
