---
layout: page
title: Documentation
permalink: /doc/
weight: 3
---

## Compiler Support

Since AdaYaml is written in Ada 2012, you can currently only use it in
conjunction with the [GNAT][1] compiler; there are no other compilers available
supporting Ada 2012. Since GNAT is the only viable compiler anyway, the AdaYaml
source makes use of some GNAT-specific features.

Moreover, AdaYaml integrates with other tools of the GNAT environment,
particularly the build system [GPRbuild][2]. You can easily define AdaYaml as
dependency of your project using GPRbuild, and you can easily install AdaYaml on
your system using GPRinstall. This is all described in detail in the GPRbuild
documentation.

## API Concepts

### Memory Management

AdaYaml uses reference counting for managing resources allocated on the heap.
Since reference counting makes it necessary to use smart pointers instead of raw
access types, AdaYaml uses naming conventions derived from [Rosen '95][3] to
communicate the nature of these smart pointer types:

 * A type named `Reference` or suffixed with `_Reference` always is a reference
   to a reference-counted heap object. It is always a tagged type and always has
   a function `Value` which retrieves an `Accessor` type to access the
   underlying object. The `Accessor` type is used to [prevent deallocation][4]
   and always has the `Implicit_Dereference` aspect. Copying a `Reference` type
   always performs a shallow copy.
 * A type named `Instance` typically allocates its content on the stack
   (if used as a stack variable), but may contain references to heap values. If
   a `Reference` type exists wrapping an `Instance` type, using the `Reference`
   type is generally optional. If a subroutine, particularly in another package,
   may take co-ownership of an instance, it will require a `Reference` as
   parameter type.
 * A type named `Pointer` is a raw access type to heap memory. A subroutine
   taking a value of such a type is guaranteed to take ownership of the value
   and deallocate it once it is not needed anymore.

The API has been designed so that a user does never need to actively deallocate
anything, minimizing the danger of memory leaks.

### Event Streams

Most parts of the API are concerned with either producing or consuming a stream
of YAML events. A possible approach to make all consumers compatible with all
producers would be to define a tagged base type that provides an abstract
subroutine to query the next event from the stream. However, this would result
in dispatching calls for every event query. Therefore, a different approach has
been taken:

The API defines a package `Yaml.Stream_Concept` which does not contain any
content other than generic parameters for a stream producer type, its reference,
and a subroutine querying the next event. Any type that can be used to
instantiate that package is considered an event stream producer. Basic packages
like `Yaml.Parser` provide an instantiation of `Stream_Concept` as child package
(`Yaml.Parser.Stream` in this case).

Any package that either takes an instance of `Stream_Concept` as generic
parameter itself or provides a generic subroutine taking such an instance is
considered an event stream consumer. So if you e.g. want to consume the event
stream generated by a parser, you instantiate your generic consumer with the
parameter `Yaml.Parser.Stream`. A consumer provided by AdaYaml would be
`Yaml.Presenter` with its generic subroutine `Consume`.

## Example

This example code reads a file (or stdin, if no file is given on the command
line) and writes the resulting YAML events to stdout.

{% highlight ada %}
with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Source.Text_IO;
with Yaml.Source.File;
with Yaml.Parser;

procedure Print_Events is
   Input : Yaml.Source.Pointer;
   P     : Yaml.Parser.Instance;
   Cur   : Yaml.Event;
begin
   --  Input is either stdin or a file given as command line argument
   if Ada.Command_Line.Argument_Count = 0 then
      Input := Yaml.Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := Yaml.Source.File.As_Source (Ada.Command_Line.Argument (1));
   end if;

   --  The parser takes ownership of the input and will deallocate it.
   --  Note that an input must be set before events can be queried.
   P.Set_Input (Input);

   loop
      --  Fetch the next event from the parser
      Cur := P.Next;
      --  Print the event to stdout
      Ada.Text_IO.Put_Line (Yaml.To_String (Cur));
      --  If the event was a Stream_End, exit the loop (emitting Stream_End
      --  is the parser's way of saying "there are no more events".)
      exit when Cur.Kind = Yaml.Stream_End;
   end loop;
end Print_Events;
{% endhighlight %}

 [1]: http://gnuada.sourceforge.net
 [2]: https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html
 [3]: http://dl.acm.org/citation.cfm?id=224131
 [4]: http://www.adacore.com/adaanswers/gems/gem-107-preventing-deallocation-for-reference-counted-types/