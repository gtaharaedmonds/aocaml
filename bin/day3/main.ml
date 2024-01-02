open Base

type token = Digit of int | Symbol | Period | Gear
type schematic = { tokens : token array array; width : int; height : int }

let deltas =
  [ (0, 1); (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1); (-1, 0); (-1, 1) ]

let dims lines =
  match lines with
  | [] -> (0, 0)
  | first :: _ ->
      let width = String.length first in
      let height = List.length lines in
      (width, height)

let parse_token char =
  Char.(
    if is_digit char then Digit (to_int char - to_int '0')
    else if equal char '.' then Period
    else if equal char '*' then Gear
    else Symbol)

let rec parse_schematic lines schematic y =
  match lines with
  | [] -> ()
  | line :: tl ->
      for x = 0 to schematic.width - 1 do
        schematic.tokens.(y).(x) <- parse_token line.[x]
      done;
      parse_schematic tl schematic (y + 1)

let solve solve_schem file =
  let lines = In_channel.with_open_text file In_channel.input_lines in
  let width, height = dims lines in
  let schematic =
    {
      tokens =
        Array.init height ~f:(fun _ -> Array.init width ~f:(fun _ -> Period));
      width;
      height;
    }
  in
  parse_schematic lines schematic 0;
  solve_schem schematic

module Part1 = struct
  let adjacent_symbol x y schematic =
    List.exists deltas ~f:(fun (dx, dy) ->
        let x = x + dx in
        let y = y + dy in
        if x >= 0 && x < schematic.width && y >= 0 && y < schematic.height then
          match schematic.tokens.(y).(x) with
          | Symbol | Gear -> true
          | _ -> false
        else false)

  let solve_schematic schematic =
    let sum = ref 0 in
    let num = ref 0 in
    let valid = ref false in
    let sum_if_valid () =
      if !valid then sum := !sum + !num;
      num := 0;
      valid := false
    in
    for y = 0 to schematic.height - 1 do
      for x = 0 to schematic.width - 1 do
        match schematic.tokens.(y).(x) with
        | Symbol | Period | Gear -> sum_if_valid ()
        | Digit digit ->
            num := (!num * 10) + digit;
            if adjacent_symbol x y schematic then valid := true
      done;
      sum_if_valid ()
    done;
    !sum
end

module Part2 = struct
  type ratio = ZeroGear | OneGear of int | TwoGear of int | Invalid

  let adjacent_gears x y schematic =
    List.filter_map deltas ~f:(fun (dx, dy) ->
        let x = x + dx in
        let y = y + dy in
        if x >= 0 && x < schematic.width && y >= 0 && y < schematic.height then
          match schematic.tokens.(y).(x) with Gear -> Some (x, y) | _ -> None
        else None)

  let rec dedup_pair l =
    match l with
    | [] -> []
    | (x1, y1) :: tl ->
        let is_dup = List.exists tl ~f:(fun (x2, y2) -> x1 = x2 && y1 = y2) in
        if is_dup then dedup_pair tl else (x1, y1) :: dedup_pair tl

  let add_gear ratio num =
    match ratio with
    | Invalid -> Invalid
    | ZeroGear -> OneGear num
    | OneGear r -> TwoGear (r * num)
    | TwoGear _ -> Invalid

  let solve_schematic schematic =
    let num = ref 0 in
    let gears = ref [] in
    let ratios =
      Array.init schematic.height ~f:(fun _ ->
          Array.init schematic.width ~f:(fun _ -> ZeroGear))
    in
    let update_ratios () =
      !gears |> dedup_pair
      |> List.iter ~f:(fun (x, y) ->
             ratios.(y).(x) <- add_gear ratios.(y).(x) !num);
      gears := [];
      num := 0
    in
    for y = 0 to schematic.height - 1 do
      for x = 0 to schematic.width - 1 do
        match schematic.tokens.(y).(x) with
        | Symbol | Period | Gear -> update_ratios ()
        | Digit digit -> (
            num := (!num * 10) + digit;
            gears :=
              match adjacent_gears x y schematic with
              | [] -> !gears
              | new_gears -> List.append !gears new_gears)
      done;
      update_ratios ()
    done;

    Array.fold ratios ~init:0 ~f:(fun acc arr ->
        acc
        + Array.fold arr ~init:0 ~f:(fun acc ratio ->
              match ratio with TwoGear ratio -> acc + ratio | _ -> acc))
end

let _ = solve Part1.solve_schematic "data/day3.txt" |> Stdio.printf "%d\n"
let _ = solve Part2.solve_schematic "data/day3test.txt" |> Stdio.printf "%d\n"
