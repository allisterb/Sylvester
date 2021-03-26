﻿(*

Copyright 2014 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace Sylvester

/// Immutable multisets. Elements are ordered by F# generic comparison.
type Multiset<'T when 'T : comparison> = Map<'T, uint32>

(*
/// Functional programming operators related to the Multiset<_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Multiset =
    /// The empty multiset.
    [<CompiledName("Empty")>]
    let empty : Multiset<'T> =
        Map.empty

    /// Is the multiset empty?
    [<CompiledName("IsEmpty")>]
    let isEmpty (set : Multiset<'T>) = Map.isEmpty set

    /// The total number of elements in the multiset.
    [<CompiledName("Count")>]
    let count (set : Multiset<'T>) : int64 =
        (0L, set)
        ||> Map.fold (fun count _ cardinality ->
            Checked.(+) (int64 cardinality) count)

    /// The number of distinct elements in the multiset.
    [<CompiledName("CountDistinct")>]
    let countDistinct (set : Multiset<'T>) = Map.count set

    //
    [<CompiledName("Contains")>]
    let contains value (set : Multiset<'T>) =
        
        Map.containsKey value set

    //
    [<CompiledName("Cardinality")>]
    let card value (set : Multiset<'T>) =
        // Preconditions
       
        set
        |> Map.tryFind value
        //|> Option. 0u

    //
    [<CompiledName("Singleton")>]
    let singleton value : Multiset<'T> =
        Map.singleton value 1u

    //
    [<CompiledName("Add")>]
    let add value (set : Multiset<'T>) : Multiset<'T> =
        // Preconditions
        checkNonNull "set" set

        // If the underlying map already contains the value as a key, increment the value's cardinality
        // and return; otherwise, add a new entry to the map for this value.
        match Map.tryFind value set with
        | None ->
            Map.add value 1u set
        | Some card ->
            Map.add value (Checked.(+) card 1u) set

    //
    [<CompiledName("AddMany")>]
    let addMany value count (set : Multiset<'T>) : Multiset<'T> =
        // Preconditions
        checkNonNull "set" set
        if count < 0 then
            argOutOfRange "count" "Cannot add a negative number of elements to the set."

        // If no elements are being added, just return the input set.
        if count = 0 then set
        else
            // If the underlying map already contains the value as a key, increment the value's cardinality by the specified count
            // and return; otherwise, add a new entry to the map for this value.
            match Map.tryFind value set with
            | None ->
                Map.add value (uint32 count) set
            | Some card ->
                Map.add value (Checked.(+) card (uint32 count)) set

    //
    [<CompiledName("Remove")>]
    let remove value (set : Multiset<'T>) : Multiset<'T> =
        // Preconditions
        checkNonNull "set" set

        match Map.tryFind value set with
        | None ->
            set
        | Some card ->
            // If cardinality is 1, we decrement to zero by removing the element from the set.
            if card = 1u then
                Map.remove value set
            else
                Map.add value (card - 1u) set

    //
    [<CompiledName("RemoveMany")>]
    let removeMany value count (set : Multiset<'T>) : Multiset<'T> =
        // Preconditions
        checkNonNull "set" set
        if count < 0 then
            argOutOfRange "count" "Cannot remove a negative number of elements from the set."

        match Map.tryFind value set with
        | None ->
            set
        | Some card ->
            // If cardinality is <= the specified number of copies of this element to remove,
            // remove the element from the set altogether.
            if card <= uint32 count then
                Map.remove value set
            else
                Map.add value (card - uint32 count) set

    //
    [<CompiledName("RemoveAll")>]
    let removeAll value (set : Multiset<'T>) : Multiset<'T> = Map.remove value set

    *)