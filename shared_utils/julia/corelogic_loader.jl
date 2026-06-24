# CoreLogic loader (Julia) — DuckDB.jl thin wrapper.
#
# Mirrors the R loader contract (shared_utils/R/corelogic_loader.R).
# This module only reads source data; project scripts control any writes.
#
# Default parquet store: <repo_root>/data/corelogic_extracts/by_state/
#
# Usage:
#   include("shared_utils/julia/corelogic_loader.jl")
#   df = load_ot(states = ["OH"], years = 2018:2024)

using DuckDB
using DataFrames

function _repo_root()::String
    p = abspath(@__FILE__)
    d = dirname(p)
    while d != dirname(d)  # walk up until we hit filesystem root
        if isfile(joinpath(d, "README.md")) && isdir(joinpath(d, "shared_utils"))
            return d
        end
        d = dirname(d)
    end
    error("Could not locate repo root (README.md and shared_utils not found upward).")
end

function _default_parquet_root()::String
    return joinpath(_repo_root(), "data", "corelogic_extracts")
end

function _quote_strings(xs)
    return join(["'$x'" for x in xs], ", ")
end

function _quote_idents(xs)
    seen = Set{String}()
    quoted = String[]
    for c in xs
        if !(c in seen)
            push!(quoted, "\"$c\"")
            push!(seen, c)
        end
    end
    return join(quoted, ", ")
end

"""
    load_ot(; states = nothing, years = nothing, columns = nothing, sample = false, parquet_root = nothing)

Load CoreLogic Owner Transfer (transactions) data. Returns a DataFrame.
"""
function load_ot(; states = nothing, years = nothing, columns = nothing,
                   sample::Bool = false, parquet_root = nothing)
    root = isnothing(parquet_root) ? _default_parquet_root() : String(parquet_root)

    if sample
        sample_path = joinpath(root, "ot_sample_10k.parquet")
        isfile(sample_path) || error("Sample not found: $sample_path. Generate via Phase 6.")
        con = DBInterface.connect(DuckDB.DB, ":memory:")
        sel = isnothing(columns) ? "*" : _quote_idents(columns)
        df = DataFrame(DBInterface.execute(con, "SELECT $sel FROM read_parquet('$sample_path')"))
        DBInterface.close!(con)
        return df
    end

    ot_glob = replace(joinpath(root, "by_state", "ot", "**", "*.parquet"), "\\" => "/")
    sel = isnothing(columns) ? "*" : _quote_idents(vcat(columns, ["state", "year"]))
    where = String[]
    if !isnothing(states); push!(where, "state IN (" * _quote_strings(states) * ")"); end
    if !isnothing(years);  push!(where, "year IN ("  * join(years, ", ")          * ")"); end
    where_clause = isempty(where) ? "" : " WHERE " * join(where, " AND ")

    query = "SELECT $sel FROM read_parquet('$ot_glob', hive_partitioning = 1)$where_clause"
    con = DBInterface.connect(DuckDB.DB, ":memory:")
    df = DataFrame(DBInterface.execute(con, query))
    DBInterface.close!(con)
    return df
end

"""
    load_prop(; states = nothing, columns = nothing, sample = false, parquet_root = nothing)

Load CoreLogic Property Characteristics data.
"""
function load_prop(; states = nothing, columns = nothing,
                     sample::Bool = false, parquet_root = nothing)
    root = isnothing(parquet_root) ? _default_parquet_root() : String(parquet_root)

    if sample
        sample_path = joinpath(root, "prop_sample_10k.parquet")
        isfile(sample_path) || error("Sample not found: $sample_path. Generate via Phase 6.")
        con = DBInterface.connect(DuckDB.DB, ":memory:")
        sel = isnothing(columns) ? "*" : _quote_idents(columns)
        df = DataFrame(DBInterface.execute(con, "SELECT $sel FROM read_parquet('$sample_path')"))
        DBInterface.close!(con)
        return df
    end

    prop_glob = replace(joinpath(root, "by_state", "prop", "**", "*.parquet"), "\\" => "/")
    sel = isnothing(columns) ? "*" : _quote_idents(vcat(columns, ["state"]))
    where = String[]
    if !isnothing(states); push!(where, "state IN (" * _quote_strings(states) * ")"); end
    where_clause = isempty(where) ? "" : " WHERE " * join(where, " AND ")

    query = "SELECT $sel FROM read_parquet('$prop_glob', hive_partitioning = 1)$where_clause"
    con = DBInterface.connect(DuckDB.DB, ":memory:")
    df = DataFrame(DBInterface.execute(con, query))
    DBInterface.close!(con)
    return df
end
