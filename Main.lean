import Bloom

def greet (name: String) := do
    puts s!"Hello {name}!"

def main : IO Unit
  := runDefault <| asks "What is your name?" >>= greet
