let is_digit chr = chr >= '0' && chr <= '9'
let to_digit chr = int_of_char chr - int_of_char '0'

let solve file fn =
  In_channel.with_open_text file In_channel.input_lines
  |> List.map fn |> List.fold_left ( + ) 0

module Part1 = struct
  let decode line =
    (* find the digit char in str, starting at idx and stepping in the step directon *)
    let rec find_digit idx step str =
      let chr = String.get str idx in
      if is_digit chr then to_digit chr else find_digit (idx + step) step str
    in

    let first_digit = find_digit 0 1 line in
    let last_digit = find_digit (String.length line - 1) (-1) line in
    (10 * first_digit) + last_digit
end

module Part2 = struct
  let digits =
    [
      ("one", 1);
      ("two", 2);
      ("three", 3);
      ("four", 4);
      ("five", 5);
      ("six", 6);
      ("seven", 7);
      ("eight", 8);
      ("nine", 9);
    ]

  let digit_substr i str =
    digits
    |> List.find_map (fun digit ->
           let name, num = digit in
           let max_allowed_len = String.length str - i in
           let sub_len = min (String.length name) max_allowed_len in
           let substr = String.sub str i sub_len in
           if name = substr then Some num else None)

  let decode line =
    let rec find_digit i step str =
      let chr = String.get str i in
      if is_digit chr then to_digit chr
      else
        match digit_substr i str with
        | Some num -> num
        | None -> find_digit (i + step) step str
    in

    let first_digit = find_digit 0 1 line in
    let last_digit = find_digit (String.length line - 1) (-1) line in
    (10 * first_digit) + last_digit
end

let p1_soln = solve "data/day1.txt" Part1.decode
let p2_soln = solve "data/day1.txt" Part2.decode
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" p1_soln p2_soln
