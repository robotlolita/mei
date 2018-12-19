#### Generate proper help messages

This requires:

- Reading the tags from the union types to provide better information; 
- A way of describing aliases, defaults, and short descriptions for each option;
- A way to describe preludes and epilogues for parsers;
- A way to describe usage examples;

Most of this information can be handled by attributes, but some are quite long, and would be better handled by implementing an interface. We can use attributes as a convenience for the cannonical interface methods, since they don't work in Fable.


#### Generate better error messages

These should include a command-like form showing where the command errored, and what's missing from it. There should be enough context in the parser errors to build this.

Usage description and examples(if they exist) should be appended as well.


#### Support capturing arguments

Some CLIs use `--` to denote that the following arguments shouldn't be parsed, but kept as-is. This can be supported with a new type that just captures the remaining, unparsed arguments.


#### Support dynamic arguments

Some CLIs allow runtime extensions to the commands. We can't really model this statically, but it's possible to support it with a concept of combinable parsers. For this we need a Parser type that provides a way of handling its many cases: what to do with the (typed) parsed result, how to handle errors, etc; and a new parse specification type that tells us to parse-according-to-a-CLI-parser, effectively embedding a typed specification in another, but along with information about what to do with it.