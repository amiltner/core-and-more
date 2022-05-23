module type LogLevel =
sig
  include Util.Data
end

module Create(L : LogLevel) =
struct
  module Set = HashSetWrapper.HSWrapper(L)

  let store = Set.empty ()

  let register
      (l:L.t)
    : unit =
    Set.add l store

  let print
      (l:L.t)
    : (unit -> string) -> unit =
    let lstr = L.show l in
    fun string_thunk ->
      let string_val = string_thunk () in
      let full_string =
        Printf.sprintf
          "%s, Level: %s, Message: %s"
          (Core.Time.to_string (Core.Time.now ()))
          lstr
          string_val
      in
      print_endline full_string

  let log
      (l:L.t)
    : (unit -> string) -> unit =
    if Set.contains l store then
      let partial_print = print l in
      fun string_thunk ->
        partial_print string_thunk
    else
      fun _ -> ()
end
