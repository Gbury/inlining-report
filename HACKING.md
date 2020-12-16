
# Hacking OCIR

## Ocir binary

TODO: use cmdliner to provide a correct user interface, and when
      `ocir_core` is completed with more functionality, provide
      them in the CLI options (e.g. comparing two reports).


## Ocir Core

### Goal

The `ocir_core` library's goal is to define a notion of inlining reports
that can be used to represent all the inlining reports/decisions that
can be made by `ocamlopt`. Very importantly, this includes past versions
of closure/flambda, given that it will be interesting to compare the
behaviour of various successive versions of flambda.

The type definitions in this core library are thus expected to be subject
to changes, particularly when some upgrades are made to flambda.

Currently, the format is mainly geared towards the flambda2 current notion
of inlining reports, that tries and imitate the behaviour of Closure. It is
planned in the near future to extend that to also include flambda1's inlining
reports. Interestingly, it is expected that after integrating flambda1, the
types will change significantly and the cases that flambda1 and flambda2 (in
closure imitaiton mode) have in common will be shared.


## Ocir Format

### Goal

The `ocir_format` sub-library's goal is to read **all** versions of machine
readable inlining reports, and convert them to the definition of inlining
reports in `ocir_core`. The reason behind keeping the ability to read all
versions of inlining reports and not simply the most recent is to enable
comparisons between "old" versions of `ocamlopt` and "next" ones. Thus,
the serialized inlining reports must support versioning.

### Code organization

Each version format of inlining reports is defined in a separate library
called `Ocir_format_version_name`, whose code resides in directory
`src/format/version_name`.

An `ocir_format` library also exists in `src/format` to handle the version
reading and automatic dispatching to the correct specific version library,
so that the rest of the code can read any inlining report file with a single
function call.


### Serialization formats

Currently, the only serialization scheme used is to marshall/unmarshall
inlining reports. In the future, it's also planned to use json and/or
s-expressions.

Thanksfull, all of these format allow to version the inliing reports in
some way. For json and s-expr, a first object (or field) containing a
unique version string should be enough. For marshalling, the easiest
solution was to use polymorphic variants with the version name/string
encoded as the name of the variant constructor, while staying as
type-safe as reasonably possible.


### Desing choices

First is to note that all the considered serialization format support
deriving their serializer/deserializer from the type definitions[1].
Second, in order to ensure that each versioned format is correctly
read and understood, the relevant type definitions from the ocaml
compiler have to be *manually* copied into the `ocir` repo, since
they may change in later versions of the compiler.

For various reasons[2], it is better that only the serializers
for the inlining reports are included in the compiler code, the
deserializers being exclusively in the `ocir` library. As such,
and to better ensure compatibility between serializing and
deserializing, the idea would be to generate both serializer and
deserializers in `ocir`, typically using some `ppx` derivers, and
the `[@@deriving_inline]` ability of dune, to generate code for the
serializer, and then copy that code into the compiler code base.
In order to help with that method, it is better if the type definitions
of the inlining reports copied into the `ocir` repository thus reside
as much as possible in modules with the same name/path than in the compiler
(modulo the `ocir` library prefix path). This will help avoid having to
rename module names when copying serializer code. This can result in some
weird code in some instances, but should be better than the alternatives.


[1]: marshall is builtin in the compiler,
     json had `ppx_deriving_yojson` and `ppx_yojson_conv`,
     and s-exp has `ppx_sexp_conv.

[2]: deserializer are typically more complex (they can typically account
     for re-ordering of fields for json formats, etc..), and moreover would
     tecnically be dead code in the compiler. Additionally, even if the
     "official" way to deserialize reports was to go through compiler-libs,
     that would only enable reading the current version format and not all
     of them, so the deserializing code would still need to be copied in `ocir`.
     In the end, it appears far simpler to only have the serializer code in the
     compiler to minimize the added code to the compiler code base.




