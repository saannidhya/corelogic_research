"""CoreLogic loader (Python) — duckdb thin wrapper around the partitioned parquet store.

Mirrors the R loader contract (`shared_utils/R/corelogic_loader.R`).
This module only reads source data; project scripts control any writes.

Default parquet store: <repo_root>/data/corelogic_extracts/by_state/
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable

import duckdb
import polars as pl


def _repo_root() -> Path:
    """Locate the repo root by walking up to ordinary repository markers."""
    p = Path(__file__).resolve()
    for parent in p.parents:
        if (parent / "README.md").exists() and (parent / "shared_utils").exists():
            return parent
    raise RuntimeError("Could not locate repo root (README.md and shared_utils not found upward).")


def _default_parquet_root() -> Path:
    return _repo_root() / "data" / "corelogic_extracts"


def _quote(values: Iterable[str]) -> str:
    return ", ".join(f"'{v}'" for v in values)


def _quote_idents(cols: list[str]) -> str:
    """Quote column identifiers for duckdb (double-quote, dedup)."""
    seen = set()
    quoted = []
    for c in cols:
        if c not in seen:
            quoted.append(f'"{c}"')
            seen.add(c)
    return ", ".join(quoted)


def load_ot(
    states: list[str] | None = None,
    years: list[int] | None = None,
    columns: list[str] | None = None,
    sample: bool = False,
    parquet_root: Path | str | None = None,
) -> pl.DataFrame:
    """Load CoreLogic Owner Transfer (transactions) data.

    Parameters mirror the R loader: states + years + columns are pushdown filters.
    Returns a polars DataFrame.
    """
    root = Path(parquet_root) if parquet_root else _default_parquet_root()

    if sample:
        sample_path = root / "ot_sample_10k.parquet"
        if not sample_path.exists():
            raise FileNotFoundError(
                f"Sample not found at {sample_path}. "
                "Generate via Phase 6 of the workflow plan."
            )
        df = pl.read_parquet(sample_path)
        if columns:
            df = df.select(columns)
        return df

    ot_glob = str(root / "by_state" / "ot" / "**" / "*.parquet").replace("\\", "/")

    select_cols = "*" if columns is None else _quote_idents(columns + ["state", "year"])
    where = []
    if states:
        where.append(f"state IN ({_quote(states)})")
    if years:
        where.append(f"year IN ({', '.join(str(y) for y in years)})")
    where_clause = f" WHERE {' AND '.join(where)}" if where else ""

    query = (
        f"SELECT {select_cols} FROM read_parquet('{ot_glob}', "
        f"hive_partitioning = 1){where_clause}"
    )
    return duckdb.sql(query).pl()


def load_prop(
    states: list[str] | None = None,
    columns: list[str] | None = None,
    sample: bool = False,
    parquet_root: Path | str | None = None,
) -> pl.DataFrame:
    """Load CoreLogic Property Characteristics data."""
    root = Path(parquet_root) if parquet_root else _default_parquet_root()

    if sample:
        sample_path = root / "prop_sample_10k.parquet"
        if not sample_path.exists():
            raise FileNotFoundError(
                f"Sample not found at {sample_path}. "
                "Generate via Phase 6 of the workflow plan."
            )
        df = pl.read_parquet(sample_path)
        if columns:
            df = df.select(columns)
        return df

    prop_glob = str(root / "by_state" / "prop" / "**" / "*.parquet").replace("\\", "/")

    select_cols = "*" if columns is None else _quote_idents(columns + ["state"])
    where = []
    if states:
        where.append(f"state IN ({_quote(states)})")
    where_clause = f" WHERE {' AND '.join(where)}" if where else ""

    query = (
        f"SELECT {select_cols} FROM read_parquet('{prop_glob}', "
        f"hive_partitioning = 1){where_clause}"
    )
    return duckdb.sql(query).pl()
