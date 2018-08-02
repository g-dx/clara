# Introduction

Clara is a statically typed, general purpose, natively compiled language in the spirit of
[Go](https://golang.org/) which aims to be fun & clear.

For an idea of where it _might_ be going, see the [What's Next](#whats-next) section.

## Getting Started

To get Clara running on your machine you'll need the following available on your `$PATH`:

 * Go including `$GOPATH/bin`
 * GCC or Clang

Then if you are on Linux or Mac simply run the following:

<pre>
  <code class="language-bash">
    git clone https://github.com/g-dx/clara/
    cd clara
    ./build-and-run hello.clara
  </code>
</pre>

This will build the compiler, run all tests, prepare the standard library, compile the _./install/examples/hello.clara_ program 
and run it. All being well you should see the familiar "Hello World!" message in your console.

## Architecture

The following diagram shows how the Clara compiler & GCC/Clang work together to produce platform native binaries.

![Compiler Architecture](assets/images/arch.png)

Currently only OSX & Linux are supported but Windows is hopefully coming soon!  

# Overview

## Influences

Clara's heritage is mixed but can largely be traced to the following languages:

 * Go - the compiler implementation language.
 * [Koka](https://koka-lang.github.io/koka/doc/kokaspec.html) - a _function orientated_ language from Microsoft Research 
 which _infers_ side-effects.
 * Java - the first programming language I worked with. 
 * [Kotlin](https://kotlinlang.org/) - the reason Clara has had no `null`  value from the start.

## Hello World <a name="hello"></a>
As usual we start with the obligatory _hello world_ program:
<pre>
  <code class="language-clara">
    fn main() {
        printf("Hello World!\n")
    }
  </code>
</pre>

## Values

// TODO

## Functions

Clara maintains a clean separation between code & data. This means there is no way to couple a function to 
any data other than its inputs or output. 

Function declarations are fairly straightforward. The following computes the fibonacci sequence: 

<pre>
  <code class="language-clara">    
    fn fib(n: int) int {
        if n < 2 {
            return n
        }
        return fib(n - 1) + fib(n - 2)
    }
  </code>
</pre>

The `fib` function accepts a single parameter `n` of type `int` & returns an `int`. 

Functions which do not return anything are said to return `nothing` and declaring the return type is optional:

<pre>
  <code class="language-clara">    
    fn hello(msg: string) {
        printf("Hello message is: %s\n", msg)
    }
  </code>
</pre>

Functions which return the result of a single _expression_ may use the _function expression_ syntax and omit the return 
keyword & curly braces:

<pre>
  <code class="language-clara">    
    fn product(x: int, y: int, z: int) int = x * y * z
  </code>
</pre>

#### Dot Selection 

Functions can be invoked in the normal way by passing all required arguments but may also be invoked using 
"dot selection" notation. The following example illustrates this:

<pre>
  <code class="language-clara">
    fn main() {
        add(2, 3)
        2.add(3)  // Equivalent to the line above 
    }
    
    fn add(x: int, y: int) int = x + y
  </code>
</pre>

Here it important to note that the second `add` function call is simply "syntactic sugar" for the 
first `add` call. These are functionally equivalent. This allows for easy function chaining & also "extending" foreign 
types by simply writing a new function which takes that type as its first argument:

<pre>
  <code class="language-clara">
    fn main() {
        printf("%s\n", "Clara!".bold())
    }
    
    fn bold(s: string) string = "**".append(s).append("**")    
  </code>
</pre> 

#### First Class Functions

Functions in Clara are also said to be "first class" in that they can be passed as arguments to other functions, 
returned as values from functions & assigned to local variables or stored in data structures.

<pre>
  <code class="language-clara">
    fn main() {
        msg := "Down with this sort of thing. Careful now."
        x := printf
        x("%s\n", msg)
        x("%s\n", apply(bold, msg)
        x("%s\n", apply(italic, msg)
    }
    
    fn apply(f: fn(s: string) string, s: string) = f(s)
    fn bold(s: string) string = "**".append(s).append("**")
    fn italics(s: string) string = "_".append(s).append("_")
  </code>
</pre> 
 
Clara also supports both anonymous functions and closures: 

<pre>
  <code class="language-clara">
    fn main() {
        square := fn(x: int) int = x * x
        printf("6² = %d\n", square(6))
        
        c := counter(10)
        printf("%d\n", c()) // 11
        printf("%d\n", c()) // 12
        printf("%d\n", c()) // ...   
    }
    
    fn counter(x: int) fn() int {
        return fn() int {
            x = x + 1
            return x
        }
    }
  </code>
</pre>
 
## Structs

Structs represent an aggregate data type over other types, including other structs. Here is an example of a `employee` 
struct:
<pre>
  <code class="language-clara">
    struct employee {
        name: string
        age: int
        department: string
        active: bool
    }
    
    fn main() {
        e := Employee("Clark Kent", 35, "Journalism", true)
    }
  </code>
</pre>
All `struct`s automatically come with _constructor_ functions to create instances of a  

## Enums

// TODO!

## Garbage Collection

Clara contains a a _precise_, _non-moving_, _incremental_ mark & sweep garbage collector built on top of the 
[Tag-Free Garbage Collection](https://cs.nyu.edu/goldberg/pubs/gold91.pdf) scheme. This approach is interesting in that 
the pointer information gathered during compilation is used to generate _program specific_ garbage collection routines 
such that no interpretation of memory locations at runtime is required. In this sense every program contains a 
_completely bespoke_ garbage collector.  

For a thorough explanation see the linked paper.

# What's Next <a name="whats-next"></a>

The following is a list, in no particular order, of features which are slated for inclusion in the language.

 * Modules
 * Nullable Types
 * Default Arguments
 * Polymorphic Types
 * ToString() & Equals()
 * Interpolated Strings 

<div>Icons made by <a href="http://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>