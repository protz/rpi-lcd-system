open Lcd

let cgram_state = Array.make 8 0;;

let draw_caml_logo () =
  let chars = [
    [
      0b00000;
      0b00111;
      0b01111;
      0b00011;
      0b00001;
      0b00001;
      0b00000;
      0b00000;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00011;
      0b11111;
      0b11111;
      0b11111;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b11110;
      0b11111;
      0b11111;
      0b11111;
      0b11111;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b10000;
      0b11000;
      0b10000;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
    ];

    [
      0b00011;
      0b00011;
      0b00011;
      0b00010;
      0b00010;
      0b00011;
      0b00110;
      0b00000;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00001;
      0b00000;
      0b00000;
    ];

    [
      0b10000;
      0b10000;
      0b11000;
      0b10000;
      0b10000;
      0b10000;
      0b00000;
      0b00000;
    ]
  ] in
  List.iteri (fun i c -> LCD.new_char i c) chars;
  LCD.message "\000\001\002\003";
  LCD.move_cursor_abs (0, 1);
  LCD.message "\004\005\006\007";
  LCD.move_cursor_abs (6, 0);
  LCD.message "OCaml";
  LCD.move_cursor_abs (8, 1);
  LCD.message "FTW";
;;

let accents_chars =
  [|
    [
      0b00000000;
      0b00001110;
      0b00001001;
      0b00001001;
      0b00011101;
      0b00001001;
      0b00001001;
      0b00001110
    ];
    [
      0b00001101;
      0b00010010;
      0b00000000;
      0b00010001;
      0b00011001;
      0b00010101;
      0b00010011;
      0b00010001
    ];
    [
      0b00001000;
      0b00000100;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110
    ];
    [
      0b00000000;
      0b00000100;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110
    ];
    [
      0b00000000;
      0b00000100;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001
    ];
    [
      0b00000000;
      0b00001101;
      0b00010010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001
    ];
    [
      0b00000000;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001
    ];
    [
      0b00000000;
      0b00000000;
      0b00000000;
      0b00010001;
      0b00001010;
      0b00000100;
      0b00001010;
      0b00010001
    ];
    [
      0b00000000;
      0b00001110;
      0b00000100;
      0b00001110;
      0b00010101;
      0b00001110;
      0b00000100;
      0b00001110
    ];
    [
      0b00001000;
      0b00000100;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110
    ];
    [
      0b00000010;
      0b00000100;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110
    ];
    [
      0b00000100;
      0b00001010;
      0b00000000;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110
    ];
    [
      0b00001010;
      0b00000000;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110
    ];
    [
      0b00000000;
      0b00000010;
      0b00000100;
      0b00010001;
      0b00001010;
      0b00000100;
      0b00000100;
      0b00000100
    ];
    [
      0b00011000;
      0b00001000;
      0b00001110;
      0b00001001;
      0b00001001;
      0b00001110;
      0b00001000;
      0b00011100
    ];
    [
      0b00000000;
      0b00000110;
      0b00001001;
      0b00001001;
      0b00001110;
      0b00001001;
      0b00001001;
      0b00010110
    ];
    [
      0b00001000;
      0b00000100;
      0b00000000;
      0b00001110;
      0b00000001;
      0b00001111;
      0b00010001;
      0b00001111
    ];
    [
      0b00000010;
      0b00000100;
      0b00000000;
      0b00001110;
      0b00000001;
      0b00001111;
      0b00010001;
      0b00001111
    ];
    [
      0b00000100;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00000001;
      0b00001111;
      0b00010001;
      0b00001111
    ];
    [
      0b00001101;
      0b00010010;
      0b00000000;
      0b00001110;
      0b00000001;
      0b00001111;
      0b00010001;
      0b00001111
    ];
    [
      0b00000000;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00000001;
      0b00001111;
      0b00010001;
      0b00001111
    ];
    [
      0b00000100;
      0b00001010;
      0b00000100;
      0b00001110;
      0b00000001;
      0b00001111;
      0b00010001;
      0b00001111
    ];
    [
      0b00000000;
      0b00000000;
      0b00011010;
      0b00000101;
      0b00001111;
      0b00010100;
      0b00010101;
      0b00001010
    ];
    [
      0b00000000;
      0b00000000;
      0b00001110;
      0b00010000;
      0b00010001;
      0b00001110;
      0b00000100;
      0b00001100
    ];
    [
      0b00001000;
      0b00000100;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00011111;
      0b00010000;
      0b00001110
    ];
    [
      0b00000010;
      0b00000100;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00011111;
      0b00010000;
      0b00001110
    ];
    [
      0b00000100;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00011111;
      0b00010000;
      0b00001110
    ];
    [
      0b00000000;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00011111;
      0b00010000;
      0b00001110
    ];
    [
      0b00001000;
      0b00000100;
      0b00000000;
      0b00000100;
      0b00001100;
      0b00000100;
      0b00000100;
      0b00001110
    ];
    [
      0b00000010;
      0b00000100;
      0b00000000;
      0b00000100;
      0b00001100;
      0b00000100;
      0b00000100;
      0b00001110
    ];
    [
      0b00000100;
      0b00001010;
      0b00000000;
      0b00000100;
      0b00001100;
      0b00000100;
      0b00000100;
      0b00001110
    ];
    [
      0b00000000;
      0b00001010;
      0b00000000;
      0b00000100;
      0b00001100;
      0b00000100;
      0b00000100;
      0b00001110
    ];
    [
      0b00010100;
      0b00001000;
      0b00010100;
      0b00000010;
      0b00001111;
      0b00010001;
      0b00001110;
      0b00000000;
    ];
    [
      0b00001101;
      0b00010010;
      0b00000000;
      0b00010110;
      0b00011001;
      0b00010001;
      0b00010001;
      0b00010001;
    ];
    [
      0b00001000;
      0b00000100;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110;
    ];
    [
      0b00000010;
      0b00000100;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110;
    ];
    [
      0b00000000;
      0b00000100;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00001110;
    ];
    [
      0b00000000;
      0b00001101;
      0b00010010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00001110;
    ];
    [
      0b00000000;
      0b00001010;
      0b00000000;
      0b00001110;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00001110;
    ];
    [
      0b00000000;
      0b00000000;
      0b00000100;
      0b00000000;
      0b00011111;
      0b00000000;
      0b00000100;
      0b00000000;
    ];
    [
      0b00000000;
      0b00000010;
      0b00000100;
      0b00001110;
      0b00010101;
      0b00001110;
      0b00000100;
      0b00001000;
    ];
    [
      0b00001000;
      0b00000100;
      0b00000000;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010011;
      0b00001101;
    ];
    [
      0b00000010;
      0b00000100;
      0b00000000;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010011;
      0b00001101;
    ];
    [
      0b00000100;
      0b00001010;
      0b00000000;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010011;
      0b00001101;
    ];
    [
      0b00000000;
      0b00001010;
      0b00000000;
      0b00010001;
      0b00010001;
      0b00010001;
      0b00010011;
      0b00001101;
    ];
    [
      0b00000000;
      0b00000010;
      0b00000100;
      0b00010001;
      0b00010001;
      0b00001111;
      0b00000001;
      0b00001110;
    ];
    [
      0b00000000;
      0b00001100;
      0b00000100;
      0b00000110;
      0b00000101;
      0b00000110;
      0b00000100;
      0b00001110;
    ];
    [
      0b00000000;
      0b00001010;
      0b00000000;
      0b00010001;
      0b00010001;
      0b00001111;
      0b00000001;
      0b00001110;
    ];
  |]
;;

let seven_segments =
  let nil = [ 0; 0; 0; 0; 0; 0; 0; 0 ] in
  let a = [
    [
      0b00111;
      0b00111;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000
    ];
    [
      0b11100;
      0b11100;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000
    ];
    nil; nil
  ] in
  let b = [
    nil; [
      0b00000;
      0b00000;
      0b00011;
      0b00011;
      0b00011;
      0b00011;
      0b00011;
      0b00000
    ];
    nil; nil
  ] in
  let c = [
    nil; nil;
    nil; [
      0b00000;
      0b00011;
      0b00011;
      0b00011;
      0b00011;
      0b00011;
      0b00000;
      0b00000
    ];
  ] in
  let d = [
    nil; nil;
    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00111;
      0b00111
    ];
    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b11100;
      0b11100
    ];
  ] in
  let e = [
    nil; nil;
    [
      0b00000;
      0b11000;
      0b11000;
      0b11000;
      0b11000;
      0b11000;
      0b00000;
      0b00000
    ]; nil
  ] in
  let f = [
    [
      0b00000;
      0b00000;
      0b11000;
      0b11000;
      0b11000;
      0b11000;
      0b11000;
      0b00000
    ]; nil; 
    nil; nil
  ] in
  let g = [
    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00111
    ];
    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b11100
    ];
    [
      0b00111;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000
    ];
    [
      0b11100;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000
    ];
  ] in
  let colon = [
    [
      0b00000;
      0b00000;
      0b00000;
      0b00011;
      0b00011;
      0b00011;
      0b00011;
      0b00000
    ];
    [
      0b00000;
      0b00000;
      0b00000;
      0b11000;
      0b11000;
      0b11000;
      0b11000;
      0b00000
    ];
    [
      0b00000;
      0b00011;
      0b00011;
      0b00011;
      0b00011;
      0b00000;
      0b00000;
      0b00000
    ];
    [
      0b00000;
      0b11000;
      0b11000;
      0b11000;
      0b11000;
      0b00000;
      0b00000;
      0b00000
    ];
  ] in
  let ( ++ ) quad1 quad2 =
    List.map2 (fun bytes1 bytes2 ->
      List.map2 (fun b1 b2 -> b1 lor b2) bytes1 bytes2
    ) quad1 quad2
  in
  [|
    a ++ b ++ c ++ d ++ e ++ f;
    b ++ c;
    a ++ b ++ g ++ e ++ d;
    a ++ b ++ g ++ c ++ d;
    f ++ b ++ g ++ c;
    a ++ f ++ g ++ c ++ d;
    a ++ f ++ c ++ d ++ e ++ g;
    a ++ b ++ c;
    a ++ b ++ c ++ d ++ e ++ f ++ g;
    a ++ b ++ c ++ d ++ f ++ g;
    colon;
  |]
;;

type block = A | B | C | D | E | F | G | H | S | Z

let blocks =
  [
  [
    0b00001;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b00001;
  ];
  [
    0b11111;
    0b01110;
    0b00000;
    0b00000;
    0b00000;
    0b00000;
    0b01110;
    0b11111;
  ];
  [
    0b11111;
    0b01111;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b01111;
    0b11111;
  ];
  [
    0b11111;
    0b11110;
    0b11000;
    0b11000;
    0b11000;
    0b11000;
    0b11110;
    0b11111;
  ];
  [
    0b11111;
    0b01110;
    0b00000;
    0b00000;
    0b00000;
    0b00000;
    0b00000;
    0b00000;
  ];
  [
    0b10000;
    0b11000;
    0b11000;
    0b11000;
    0b11000;
    0b11000;
    0b11110;
    0b11111;
  ];
  [
    0b00001;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b01111;
    0b11111;
  ];
  [
    0b11111;
    0b01111;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b00011;
    0b00001;
  ];
]
;;

let block_letters =
  [|
    [ S; H; F; G ];
    [ Z; A; Z; A ];
    [ B; C; D; B ];
    [ B; C; B; C ];
    [ F; G; E; H ];
    [ D; B; B; C ];
    [ D; B; D; C ];
    [ E; H; Z; A ];
    [ D; C; D; C ];
    [ D; C; B; C ]
  |]

let load_blocks () =
  for i = 0 to 7 do
    cgram_state.(i) <- 0;
  done;
  List.iteri LCD.new_char blocks
;;

let char_of_block = function
  | A -> '\000'
  | B -> '\001'
  | C -> '\002'
  | D -> '\003'
  | E -> '\004'
  | F -> '\005'
  | G -> '\006'
  | H -> '\007'
  | Z -> ' '
  | S -> '/'
;;

let block_digit digit pos =
  let sequence = block_letters.(digit) in
  let c1, c2, c3, c4 =
    match List.map char_of_block sequence with
    | [c1; c2; c3; c4 ] -> c1, c2, c3, c4
    | _ -> assert false
  in
  LCD.move_cursor_abs (pos, 0);
  LCD.message (Printf.sprintf "%c%c" c1 c2);
  LCD.move_cursor_abs (pos, 1);
  LCD.message (Printf.sprintf "%c%c" c3 c4);
;;

let block_colon pos =
  LCD.move_cursor_abs (pos, 0);
  LCD.message (Printf.sprintf "%c" (Char.chr 0b10100101));
  LCD.move_cursor_abs (pos, 1);
  LCD.message (Printf.sprintf "%c" (Char.chr 0b10100101));
;;


let accents_table = [
  "à", 16;
  "â", 18;
  "ä", 20;
  "ç", 23;
  "è", 24;
  "é", 25;
  "ê", 26;
  "ë", 27;
  "î", 30;
  "ï", 31;
  "ñ", 33;
  "ô", 36;
  "ö", 38;
  "ù", 41;
  "û", 43;
  "ü", 44;
];;

let fix_accents str =
  (* Replace some characters that have an equivalent on the LCD *)
  let re_known_accents = Pcre.regexp ~flags:[ `UTF8 ] "[º]" in
  let str = Pcre.substitute
    ~rex:re_known_accents
    ~subst:(function
      | "º" -> String.make 1 (Char.chr 0b11011111)
      | _ -> assert false
    )
    str
  in

  let re_accents = Pcre.regexp
    ~flags:[ `UTF8 ]
    ("[" ^ String.concat "" (List.map fst accents_table) ^ "]")
  in

  let collect_accents () =
    let accents = Array.to_list (Pcre.extract_all ~rex:re_accents str) in
    let accents = List.map (fun x -> x.(0)) accents in
    List.map (fun c -> List.assoc c accents_table) accents
  in

  let shuffle_cgram () =
    for i = 7 downto 1 do
      let j = Random.int i in
      let t = cgram_state.(i) in
      cgram_state.(i) <- cgram_state.(j);
      cgram_state.(j) <- t
    done;
    Printf.eprintf "Shuffled CGRAM\n%!";
  in

  let write_cgram () =
    for i = 0 to 7 do
      Printf.eprintf "CGRAM position %d charcode %d\n%!"
        i
        cgram_state.(i);
      LCD.new_char i (accents_chars.(cgram_state.(i)))
    done;
    Printf.eprintf "Wrote CGRAM\n%!";
  in

  let accents = collect_accents () in

  let cgram_state_l = Array.to_list cgram_state in
  if List.exists (fun c -> not (List.mem c cgram_state_l)) accents then begin
    Printf.eprintf "Re-programming CGRAM\n";
    if List.length accents >= 8 then
      failwith "Out of CGRAM";
    shuffle_cgram ();
    List.iteri (fun i c -> cgram_state.(i) <- c) accents;
    write_cgram ();
  end;

  let rev_table = List.mapi (fun i x -> x, i) (Array.to_list cgram_state) in

  Pcre.substitute ~rex:re_accents ~subst:(fun c ->
    let offset = List.assoc c accents_table in
    let cgram_pos = List.assoc offset rev_table in
    String.make 1 (Char.chr cgram_pos)
  ) str
;;
    
let fix_accents str =
  try
    fix_accents str
  with Not_found ->
    str
;;

let write_special_twodigits (i, j) pos =
  (* Clear cgram state. *)
  for i = 0 to 7 do
    cgram_state.(i) <- 0;
  done;
  let quad1 = seven_segments.(i) in
  let quad2 = seven_segments.(j) in
  List.iteri (fun i c -> LCD.new_char i c) quad1;
  List.iteri (fun i c -> LCD.new_char (i + 4) c) quad2;
  LCD.move_cursor_abs (pos, 0);
  LCD.message "\000\001\004\005";
  LCD.move_cursor_abs (pos, 1);
  LCD.message "\002\003\006\007";
;;

let capitalize s =
  let s = String.lowercase s in
  let prev_char = ref ' ' in
  let is_letter c =
    let c = Char.code c in
    Char.code 'a' <= c && c <= Char.code 'z' ||
    Char.code 'A' <= c && c <= Char.code 'Z'
  in
  for i = 0 to String.length s - 1 do
    if !prev_char = ' ' && is_letter s.[i] then
      s.[i] <- Char.uppercase s.[i];
    prev_char := s.[i];
  done;
  s
