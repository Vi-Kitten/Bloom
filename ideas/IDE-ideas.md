# Ideas for what would make a good Bloom IDE

I think a cute name would be tulip.

- Atkinson Hyperlegible as UI font.
- Partial refactoring tools that flag areas that could not be automatically changed.
- Detect the issues caused by deleting a section of code before running the deletion, potentially suggesting refactors to eliviate issues.
- Powerful refactoring options:
    - Introduce new generic parameter, univerally quantifying previous constructions where possible.
    - Specialise generic parameter at definition level, removing universal quantifications where possible.
    - Split pattern into multiple cases, making the previous pattern a matching alias and creating decisions that need to be made for case construciton.
    - Remove and inline out a definition.
    - Introduce new arg to series of functions, forwarding the arg where available, and replacing it with a typed hole otherwise `...`.
    - Parsed and Type checked / Kind checked search replace.