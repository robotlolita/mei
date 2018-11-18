module Mei.Tests.Main

// NOTE: this hack's only here because `allFiles` causes fable to try to
//       compile even the dependencies we don't use, and it fails :/
#if FABLE_COMPILER
open Fable.Core.JsInterop

importSideEffects "./Core.test.fs"
importSideEffects "./Encoding.test.fs"
importSideEffects "./Reflection.test.fs"
importSideEffects "./FullParser.test.fs"
#endif
