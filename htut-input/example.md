---
ghci: 'example-code/helloworld'
---

# Hello, World!

This is an example of a markdown document generated with htut, which documents `example-code/helloworld`.

Here's the header of the Main module, which contains everything before the first label:

~~~include:main
header
~~~

Here's the main function:

~~~include:main
main
~~~

Let's run it:

~~~ghci
main
~~~

Here's the implementation of fibs, from the fibs module:

~~~include:fibs
fibs
~~~

Let's get the first 10 fibonacci numbers:

~~~ghci
take 10 fibs
~~~

Here's another implementation, which is possibly easier to understand:

~~~{.ghci .multiline}
let fibs = go 0 1
    go a b = a : go b (a + b)
in take 10 fibs
~~~

Multiline input is buggy, but at least that worked...
