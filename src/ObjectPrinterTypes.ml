[@@@ocaml.doc
  "\n * Copyright (c) Facebook, Inc. and its affiliates.\n *\n * This source code is licensed under the MIT license found in the\n * LICENSE file in the root directory of this source tree.\n "]
module ObjectPrinter =
  struct
    type t =
      {
      int: t -> int -> string;
      string: t -> string -> string;
      quotedString: t -> string -> string;
      float: t -> float -> string;
      maxDepthExceeded: t -> string;
      maxLengthExceeded: t -> string;
      unknown: t -> Obj.t -> string;
      lazy_: t -> Obj.t -> string;
      doubleArray: t -> ?depth:int -> Obj.t -> string;
      closure: t -> Obj.t -> string;
      block: t -> ?depth:int -> Obj.t -> string;
      list: t -> ?depth:int -> Obj.t -> string;
      polymorphicPrint: 'o . t -> ?depth:int -> 'o -> string;}
  end
module type ObjectPrinter  =
  sig
    type t =
      {
      int: t -> int -> string;
      string: t -> string -> string;
      quotedString: t -> string -> string;
      float: t -> float -> string;
      maxDepthExceeded: t -> string;
      maxLengthExceeded: t -> string;
      unknown: t -> Obj.t -> string;
      lazy_: t -> Obj.t -> string;
      doubleArray: t -> ?depth:int -> Obj.t -> string;
      closure: t -> Obj.t -> string;
      block: t -> ?depth:int -> Obj.t -> string;
      list: t -> ?depth:int -> Obj.t -> string;
      polymorphicPrint: 'o . t -> ?depth:int -> 'o -> string;}
    val setPrintWidth : int -> unit[@@ocaml.doc
                                     "\n   * Set the print width of objects printed in any console that uses\n   * `ObjectPrinter`.\n   "]
    val setMaxDepth : int -> unit[@@ocaml.doc
                                   "\n   * Set the maximum depth the printer will print (default 20).\n   "]
    val setMaxLength : int -> unit[@@ocaml.doc
                                    "\n   * Set the maximum length of structures to print. Prevents crashing\n   * on circular lists. Must be greater than 2.\n   "]
    val base : t[@@ocaml.doc
                  "\n   * The base ObjectPrinter which can be customized.\n   "]
  end
