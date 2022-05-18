open Util
open My_dict
open My_hash_cons_dict

module type UnfixedDataFunction = sig
  module Arg : Data
  module Result : Data
  val f : (Arg.t -> Result.t) unfixed
end

module FixMemoizerOf(F:UnfixedDataFunction) : Container
  with type t = F.Arg.t -> F.Result.t =
struct
  module ResultDict = DictOf(F.Arg)(F.Result)

  let result_storage : ResultDict.t ref = ref ResultDict.empty

  type t = F.Arg.t -> F.Result.t

  let rec v
      (x:F.Arg.t)
    : F.Result.t =
    begin match ResultDict.lookup (!result_storage) x with
      | None ->
        let y = F.f v x in
        result_storage := ResultDict.insert (!result_storage) x y;
        y
      | Some y -> y
    end
end


module type UnfixedHCDataFunction = sig
  module Arg : UIDData
  module Result : Data
  val f : (Arg.t -> Result.t) unfixed
end

module FixHCMemoizerOf(F:UnfixedHCDataFunction) : Container
  with type t = F.Arg.t -> F.Result.t =
struct
  module ResultDict = HCDictOf(F.Arg)(F.Result)

  let result_storage : ResultDict.t ref = ref ResultDict.empty

  let clear () : unit = result_storage := ResultDict.empty

  type t = F.Arg.t -> F.Result.t

  let rec v
      (x:F.Arg.t)
    : F.Result.t =
    begin match ResultDict.lookup (!result_storage) x with
      | None ->
        let y = F.f v x in
        result_storage := ResultDict.insert (!result_storage) x y;
        y
      | Some y -> y
    end
end
