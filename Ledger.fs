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
        "*DateFormat*", "MM\/dd\/yyyy"  // TODO: Are these backslashes needed?
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

    let lookupStr key locale =
        locale |> lookupTable |> Map.find key

let padOrTrim width (str:string) =
    if str.Length <= width then
        str.PadRight(width)
    else
        str.Substring(0, width-3) + "..."

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
        let c = float x.chg / 100.0

        if c < 0.0 then 
            if locale = "nl-NL" then
                if currency = "USD" then
                    res <- res + ("$ " + c.ToString("#,#0.00", new CultureInfo("nl-NL"))).PadLeft(13) 
                if currency = "EUR" then
                    res <- res + ("€ " + c.ToString("#,#0.00", new CultureInfo("nl-NL"))).PadLeft(13) 
            if locale = "en-US" then
                if currency = "USD" then
                    res <- res + ("($" + c.ToString("#,#0.00", new CultureInfo("en-US")).Substring(1) + ")").PadLeft(13) 
                if currency = "EUR" then
                    res <- res + ("(€" + c.ToString("#,#0.00", new CultureInfo("en-US")).Substring(1) + ")").PadLeft(13) 
        else 
            if locale = "nl-NL" then
                if currency = "USD" then
                    res <- res + ("$ " + c.ToString("#,#0.00", new CultureInfo("nl-NL")) + " ").PadLeft(13) 
                if currency = "EUR" then
                    res <- res + ("€ " + c.ToString("#,#0.00", new CultureInfo("nl-NL")) + " ").PadLeft(13) 
            if locale = "en-US" then
                if currency = "USD" then
                    res <- res + ("$" + c.ToString("#,#0.00", new CultureInfo("en-US")) + " ").PadLeft(13) 
                if currency = "EUR" then
                    res <- res + ("€" + c.ToString("#,#0.00", new CultureInfo("en-US")) + " ").PadLeft(13) 
    res
