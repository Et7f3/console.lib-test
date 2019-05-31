[@@@ocaml.doc
  "\n * Copyright (c) Facebook, Inc. and its affiliates.\n *\n * This source code is licensed under the MIT license found in the\n * LICENSE file in the root directory of this source tree.\n "]
let colWidth = { contents = 70 }
let maxDepth = { contents = 70 }
let maxLength = { contents = 5 }
include ObjectPrinterTypes.ObjectPrinter
let setPrintWidth w = colWidth.contents <- w
let setMaxDepth d = maxDepth.contents <- d
let setMaxLength l = assert (l > 2); maxLength.contents <- l
let rec detectList : 'o . maxLength:int -> 'o -> bool= fun (type o) ->
  (fun ~maxLength  ->
     fun o  ->
       if maxLength == 0
       then true
       else
         (let magicO = Obj.magic o in
          let tag = Obj.tag magicO in
          match tag == Obj.int_tag with
          | true  -> magicO = (Obj.repr [])
          | false  ->
              let size = Obj.size magicO in
              (tag == Obj.first_non_constant_constructor_tag) &&
                ((size == 2) &&
                   (detectList ~maxLength:(maxLength - 1)
                      (Obj.field magicO 1)))) : maxLength:int -> o -> bool)
  [@@ocaml.doc
    "\n * TODO: You can rule out lists (which you know are homogeneous in contents)\n * by examining if one but not all of the contents are (strings, floats, or\n * other special types). If something appears to be a list apart from the\n * discovered heterogeneous content, we can print something called\n * \"list-like\".\n *\n * ~maxLength: Length to examine before making a decision. Must correspond to\n * the printing max length.\n "]
let rec extractList ~maxNum  o =
  if maxNum == 0
  then ((not (Obj.is_int o)), [])
  else
    if Obj.is_int o
    then (false, [])
    else
      (let (restWasTruncated,rest) =
         extractList ~maxNum:(maxNum - 1) (Obj.field o 1) in
       (restWasTruncated, ((Obj.field o 0) :: rest)))[@@ocaml.doc
                                                       "\n * Returns (didTruncate, extractedList).\n "]
let extractFields ~maxNum  o =
  let rec extractFields ~maxNum  fieldsSoFar numFields =
    if maxNum == 0
    then ((numFields > 0), fieldsSoFar)
    else
      if numFields == 0
      then (false, fieldsSoFar)
      else
        extractFields ~maxNum:(maxNum - 1) ((Obj.field o (numFields - 1)) ::
          fieldsSoFar) (numFields - 1) in
  extractFields ~maxNum [] (Obj.size o)
let getBreakData itms =
  let (allItemsLen,someChildBroke) =
    List.fold_left
      (fun (curTotalLen,curDidBreak)  ->
         fun itm  ->
           let containsNewline = String.contains itm '\n' in
           (((curTotalLen + (String.length itm)) + 2),
             (curDidBreak || containsNewline))) (0, false) itms in
  (allItemsLen, someChildBroke)
let rec indentForDepth n =
  match n with
  | 0 -> ""
  | 1 -> (("  ")[@reason.raw_literal "  "])
  | 2 -> (("    ")[@reason.raw_literal "    "])
  | 3 -> (("      ")[@reason.raw_literal "      "])
  | 4 -> (("        ")[@reason.raw_literal "        "])
  | 5 -> (("          ")[@reason.raw_literal "          "])
  | 6 -> (("            ")[@reason.raw_literal "            "])
  | 7 -> (("              ")[@reason.raw_literal "              "])
  | 8 -> (("                ")[@reason.raw_literal "                "])
  | _ -> (indentForDepth (n - 1)) ^ (("  ")[@reason.raw_literal "  "])
let printTreeShape pair self ~depth  (o : Obj.t) =
  let (left,right) = pair in
  let (wasTruncated,lst) = extractFields ~maxNum:(maxLength.contents) o in
  let dNext = 1 + depth in
  let indent = indentForDepth depth in
  let indentNext = indentForDepth dNext in
  let itms =
    List.map (fun o  -> self.polymorphicPrint self ~depth:dNext o) lst in
  let (allItemsLen,someChildBroke) = getBreakData itms in
  if
    ((((String.length indent) + 2) + allItemsLen) >= colWidth.contents) ||
      someChildBroke
  then
    let truncationMsg =
      match wasTruncated with
      | true  ->
          ((",\n")[@reason.raw_literal ",\\n"]) ^
            (indentNext ^ (self.maxLengthExceeded self))
      | false  -> "" in
    left ^
      ((("\n")[@reason.raw_literal "\\n"]) ^
         (indentNext ^
            ((String.concat
                (((",\n")[@reason.raw_literal ",\\n"]) ^ indentNext) itms)
               ^
               (truncationMsg ^
                  ((("\n")[@reason.raw_literal "\\n"]) ^ (indent ^ right))))))
  else
    (let truncationMsg =
       match wasTruncated with
       | true  ->
           ((", ")[@reason.raw_literal ", "]) ^ (self.maxLengthExceeded self)
       | false  -> "" in
     left ^
       ((String.concat ((", ")[@reason.raw_literal ", "]) itms) ^
          (truncationMsg ^ right)))
let printListShape self ~depth  o =
  let (wasTruncated,lst) = extractList ~maxNum:(maxLength.contents) o in
  let dNext = 1 + depth in
  let indent = indentForDepth depth in
  let indentNext = indentForDepth dNext in
  let itms =
    List.map (fun o  -> self.polymorphicPrint self ~depth:dNext o) lst in
  let (allItemsLen,someChildBroke) = getBreakData itms in
  if
    ((((String.length indent) + 2) + allItemsLen) >= colWidth.contents) ||
      someChildBroke
  then
    let truncationMsg =
      match wasTruncated with
      | true  ->
          ((",\n")[@reason.raw_literal ",\\n"]) ^
            (indentNext ^ (self.maxLengthExceeded self))
      | false  -> "" in
    (("[")[@reason.raw_literal "["]) ^
      ((("\n")[@reason.raw_literal "\\n"]) ^
         (indentNext ^
            ((String.concat
                (((",\n")[@reason.raw_literal ",\\n"]) ^ indentNext) itms)
               ^
               (truncationMsg ^
                  ((("\n")[@reason.raw_literal "\\n"]) ^
                     (indent ^ (("]")[@reason.raw_literal "]"])))))))
  else
    (let truncationMsg =
       match wasTruncated with
       | true  ->
           ((", ")[@reason.raw_literal ", "]) ^ (self.maxLengthExceeded self)
       | false  -> "" in
     (("[")[@reason.raw_literal "["]) ^
       ((String.concat ((", ")[@reason.raw_literal ", "]) itms) ^
          (truncationMsg ^ (("]")[@reason.raw_literal "]"]))))
let base =
  {
    int = (fun _self  -> fun i  -> string_of_int i);
    string = (fun _self  -> fun s  -> s);
    quotedString =
      (fun self  ->
         fun s  ->
           (("\"")[@reason.raw_literal "\\\""]) ^
             ((self.string self s) ^ (("\"")[@reason.raw_literal "\\\""])));
    float = (fun _self  -> fun f  -> string_of_float f);
    maxDepthExceeded =
      (fun self  -> (("@max-depth")[@reason.raw_literal "@max-depth"]));
    maxLengthExceeded =
      (fun self  -> (("@max-length")[@reason.raw_literal "@max-length"]));
    unknown =
      (fun _self  ->
         fun (_o : Obj.t)  -> (("~unknown")[@reason.raw_literal "~unknown"]));
    lazy_ =
      (fun _self  ->
         fun (_o : Obj.t)  -> (("~lazy")[@reason.raw_literal "~lazy"]));
    closure =
      (fun _self  ->
         fun (f : Obj.t)  ->
           (("closure(")[@reason.raw_literal "closure("]) ^
             ((string_of_int (0 + (Obj.magic (Obj.repr f)))) ^
                ((")")[@reason.raw_literal ")"])));
    doubleArray =
      (fun self  ->
         fun ?(depth= 0)  ->
           fun (o : Obj.t)  ->
             printTreeShape
               ((("[|")[@reason.raw_literal "[|"]),
                 (("|]")[@reason.raw_literal "|]"])) self ~depth o);
    block =
      (fun self  ->
         fun ?(depth= 0)  ->
           fun o  ->
             printTreeShape
               ((("{")[@reason.raw_literal "{"]),
                 (("}")[@reason.raw_literal "}"])) self ~depth o);
    list =
      (fun self  ->
         fun ?(depth= 0)  -> fun o  -> printListShape self ~depth o);
    polymorphicPrint =
      (fun self  ->
         fun ?(depth= 0)  ->
           fun o  ->
             if depth > maxDepth.contents
             then self.maxDepthExceeded self
             else
               (let oDynamic: Obj.t = Obj.magic o in
                let tag = Obj.tag oDynamic in
                if tag == Obj.string_tag
                then
                  match depth == 0 with
                  | true  -> self.string self (Obj.magic o)
                  | false  -> self.quotedString self (Obj.magic o)
                else
                  if tag == Obj.int_tag
                  then self.int self (Obj.magic o)
                  else
                    if tag == Obj.double_tag
                    then self.float self (Obj.magic o)
                    else
                      if tag == Obj.closure_tag
                      then self.closure self (Obj.magic o)
                      else
                        if tag == Obj.double_array_tag
                        then self.doubleArray self (Obj.magic o)
                        else
                          if tag == Obj.lazy_tag
                          then self.lazy_ self (Obj.magic o)
                          else
                            if detectList ~maxLength:(maxLength.contents) o
                            then self.list self ~depth (Obj.magic o)
                            else
                              if
                                tag == Obj.first_non_constant_constructor_tag
                              then self.block self ~depth (Obj.magic o)
                              else self.unknown self (Obj.magic o)))
  }
