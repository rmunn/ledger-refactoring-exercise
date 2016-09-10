module Ledger

open System
open System.Globalization

type Entry = { dat: DateTime; des: string; chg: int }

let mkEntry date description change = { dat = DateTime.Parse(date, CultureInfo.InvariantCulture); des = description; chg = change }

[<AutoOpen>]
module Resource =
    let isValidLocale = function
        | "en-US"
        | "nl-NL" -> true
        | _ -> false

    let englishStrings =
      [
        "Date", "Date"
        "Description", "Description"
        "Change", "Change"
        "*DateFormat*", "MM/dd/yyyy"
        "*NumberFormat*", "#,#0.00"
      ] |> Map.ofList

    let dutchStrings =
      [
        "Date", "Datum"
        "Description", "Omschrijving"
        "Change", "Verandering"
        "*DateFormat*", "dd-MM-yyyy"
        "*NumberFormat*", "#,#0.00"
      ] |> Map.ofList

    let lookupTable = function
        | "en-US" -> englishStrings
        | "nl-NL" -> dutchStrings
        | _ -> englishStrings // Untranslated locales default to English

    let lookupStr key locale =
        locale |> lookupTable |> Map.find key

[<AutoOpen>]
module MoneyFormatting =
    let toCurrencySymbol = function
        | "USD" -> "$"
        | "EUR" -> "€"
        | other -> other

    let fmtCurrencySymbol currency locale =
        let symbol = currency |> toCurrencySymbol
        match locale with
        | "en-US" -> symbol
        | "nl-NL" -> sprintf "%s " symbol
        | _ -> symbol // Unrecognized locales default to just using the symbol

    let fmtNegativeNumber = function
        | "en-US" -> fun (s:string) -> s.Replace("-", "") |> sprintf "(%s)" 
        | "nl-NL" -> id
        | _ -> id // Unrecognized locales default to not changing the input

    let fmtPositiveNumber = function
        | "en-US"
        | "nl-NL" -> fun s -> sprintf "%s " s
        | _ -> id // Unrecognized locales default to not changing the input

    let fmtChange currency locale (amt:int) =
        let chg = float amt / 100.0
        let symbol = fmtCurrencySymbol currency locale 
        let fmtStr = lookupStr "*NumberFormat*" locale
        let s = symbol + chg.ToString(fmtStr, new CultureInfo(locale))
        if chg >= 0.0 then fmtPositiveNumber locale s
                      else fmtNegativeNumber locale s

let padOrTrim width (str:string) =
    if str.Length <= width then
        str.PadRight(width)
    else
        str.Substring(0, width-3) + "..."

let padLeft width (str:string) = str.PadLeft(width)

let formatChangeW width currency locale amt =
    fmtChange currency locale amt |> padLeft width

let formatDate (date:DateTime) locale =
    date.ToString(lookupStr "*DateFormat*" locale)

let formatLedger currency locale entries =

    if not (isValidLocale locale) then failwith "Invalid locale" else
    let mutable res = ""

    let columns = [ "Date", 10; "Description", 25; "Change", 13 ]
    let header =
        columns
        |> List.map (fun (key,width) -> lookupStr key locale |> padOrTrim width)
        |> String.concat " | "
    res <- res + header

    for x in List.sortBy (fun x -> x.dat, x.des, x.chg) entries do

        res <- res + "\n"

        res <- res + formatDate x.dat locale

        res <- res + " | "

        res <- res + padOrTrim 25 x.des

        res <- res + " | "

        res <- res + formatChangeW 13 currency locale x.chg

    res
