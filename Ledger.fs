module Ledger

open System
open System.Globalization

type Entry = { dat: DateTime; des: string; chg: int }

let mkEntry date description change =
    { dat = DateTime.Parse(date, CultureInfo.InvariantCulture)
      des = description
      chg = change }

[<AutoOpen>]
module Resource =
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

let formatDateW width date locale =
    formatDate date locale |> padOrTrim width

let formatLedger currency locale entries =
    let dateWidth = 10
    let descWidth = 25
    let chngWidth = 13

    let header =
        [ "Date", dateWidth; "Description", descWidth; "Change", chngWidth ]
        |> List.map (fun (key,width) -> lookupStr key locale |> padOrTrim width)
        |> String.concat " | "

    let body =
        entries
        |> List.sortBy (fun x -> x.dat, x.des, x.chg)
        |> List.map (fun x ->
            let date = formatDateW dateWidth x.dat locale
            let desc = padOrTrim descWidth x.des
            let chng = formatChangeW chngWidth currency locale x.chg
            sprintf "%s | %s | %s" date desc chng)

    header :: body |> String.concat "\n"