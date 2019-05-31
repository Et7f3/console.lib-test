[@@@ocaml.doc
  "\n * Copyright (c) Facebook, Inc. and its affiliates.\n *\n * This source code is licensed under the MIT license found in the\n * LICENSE file in the root directory of this source tree.\n "]
module ObjectPrinter = ObjectPrinter
type t =
  {
  log: 'a . 'a -> unit;
  out: 'a . 'a -> unit;
  warn: 'a . 'a -> unit;
  error: 'a . 'a -> unit;
  debug: 'a . 'a -> unit;}
let makeStandardChannelsConsole (objectPrinter : ObjectPrinter.t) =
  ({
     log =
       (fun a  ->
          NativeChannels._log
            (objectPrinter.polymorphicPrint objectPrinter a));
     out =
       (fun a  ->
          NativeChannels._out
            (objectPrinter.polymorphicPrint objectPrinter a));
     debug =
       (fun a  ->
          NativeChannels._debug
            (objectPrinter.polymorphicPrint objectPrinter a));
     error =
       (fun a  ->
          NativeChannels._error
            (objectPrinter.polymorphicPrint objectPrinter a));
     warn =
       (fun a  ->
          NativeChannels._warn
            (objectPrinter.polymorphicPrint objectPrinter a))
   } : t)
let defaultGlobalConsole = makeStandardChannelsConsole ObjectPrinter.base
let currentGlobalConsole = { contents = defaultGlobalConsole }
let log a = (currentGlobalConsole.contents).log a
let out a = (currentGlobalConsole.contents).out a
let debug a = (currentGlobalConsole.contents).debug a
let warn a = (currentGlobalConsole.contents).warn a
let error a = (currentGlobalConsole.contents).error a
module Pipe =
  struct
    let log a = let () = (currentGlobalConsole.contents).log a in a
    let out a = let () = (currentGlobalConsole.contents).out a in a
    let debug a = let () = (currentGlobalConsole.contents).debug a in a
    let warn a = let () = (currentGlobalConsole.contents).warn a in a
    let error a = let () = (currentGlobalConsole.contents).error a in a
  end
