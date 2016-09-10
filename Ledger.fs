module Ledger

open System
open System.Globalization

type Entry = { dat: DateTime; des: string; chg: int }

let mkEntry date description change = { dat = DateTime.Parse(date, CultureInfo.InvariantCulture); des = description; chg = change }

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


let formatLedger currency locale entries =

    if not (Resource.isValidLocale locale) then failwith "Invalid locale" else
    let mutable res = ""

    let header = (Resource.lookupStr "Date" locale).PadRight(10) +
                 " | " +
                 (Resource.lookupStr "Description" locale).PadRight(25) +
                 " | " +
                 (Resource.lookupStr "Change" locale).PadRight(13)
    res <- res + header

    // if locale = "en-US" then res <- res + "Date       | Description               | Change       "
    // if locale = "nl-NL" then res <- res + "Datum      | Omschrijving              | Verandering  "
        
    for x in List.sortBy (fun x -> x.dat, x.des, x.chg) entries do

        res <- res + "\n"

        if locale = "nl-NL" then 
            res <- res + x.dat.ToString("dd-MM-yyyy")

        if locale = "en-US" then 
            res <- res + x.dat.ToString("MM\/dd\/yyyy")
                
        res <- res + " | "

        if x.des.Length <= 25 then 
            res <- res + x.des.PadRight(25)
        elif x.des.Length = 25 then 
            res <- res + x.des
        else 
            res <- res + x.des.[0..21] + "..."

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
