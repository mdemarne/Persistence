### AST persistence

AST persistence for [Project Palladium](http://scalamacros.org/news/2014/03/02/project-palladium.html). Along with [AST interpreter](https://github.com/scalareflect/interpreter), this is a key component of the new macro engine that defeats the separate compilation restrictions and makes macro expansions hostable in non-scalac environments (Intellij, runtime reflection, etc). Design notes can be found in the [AST Persistence](https://docs.google.com/document/d/1jcsLWf3uc_zdT8PkPnuDqdxXN8UhQWIGe_O_iL6Q754/edit) document at [Palladium Shared](https://drive.google.com/#folders/0Bxbd8B9L-XfmcE9tRFBXVjZtY0k).

### How to use

The project is in a very early stage, so it's not supposed to be useful just yet. However, if you're brave enough, we have a nightly build that publishes artifacts to Sonatype at `"org.scalareflect" % "persistence_2.11.0-RC1" % "0.1.0-SNAPSHOT"`.
