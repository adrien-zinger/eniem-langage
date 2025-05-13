# Reference Issue

Reference Issue: The modification of the right part in an assignment
should modify the left part. The issue might be huge if the text
assignation is considered as a reference too. But fortunally, I don't
think so.

Example of ref_assign.n:

```
let a = "hello"
await let b = "world"
await a = b
await { printf!("%s\n", a) }
await b = "hi"
printf!("%s\n", a) // expected 'hi'
```


## Scenario

Let take two scopes contains a variable `A` pointing to the same region:

scope 1: [(`A`, ptr1("hello"))]
scope 2: [(`A`, ptr1("hello"))]

Let say that I want to modify `A` in the scope 2. I have a temporary pointer
to `world` ptr2("world"). To be clear, if I modify just the pointer in the memory
of the scope 2 I risk to have:

scope 1: [(`A`, ptr1("hello"))]
scope 2: [(`A`, ptr2("world"))]

And `A` in the scope 1 remains unchanged.

In the other hand, if I choose to copy whats isinde ptr2 into ptr1, I'll get

scope 1: [(`A`, ptr1("world"))]
scope 2: [(`A`, ptr1("world"))]

Which is better.

Now, let say that I want in scope 2 (it would have been the same story if it was in
scope 1) `B` a copy of ptr1.

scope 1: [(`A`, ptr1("hello"))]
scope 2: [(`A`, ptr1("hello")), (`B`, ptr1("hello"))]

How do I create that? because the `write` method copy the value inside.

