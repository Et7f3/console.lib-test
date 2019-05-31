[@@@ocaml.doc
  "\n * Copyright (c) Facebook, Inc. and its affiliates.\n *\n * This source code is licensed under the MIT license found in the\n * LICENSE file in the root directory of this source tree.\n "]
external _out : string -> unit = "native_out"
external _log : string -> unit = "native_log"
external _debug : string -> unit = "native_debug"
external _error : string -> unit = "native_error"
external _warn : string -> unit = "native_warn"
