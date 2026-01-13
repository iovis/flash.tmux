# list recipes
default:
    @just --list

# Run tests with nextest
@test:
    cargo nextest run

# Install locally
@install:
    cargo install --path .

# Publish to crates.io
@publish: test
    cargo audit
    git push
    git push --tags
    cargo publish

# Run dist
dist:
    dist init

# Compile and open docs
@docs:
    cargo doc --open

# Open project in Github
open:
    gh repo view --web

# Open an evcxr console
console:
    # Use `:dep .` to load current crate
    @evcxr

# Upgrade dependencies
upgrade:
    cargo upgrade --incompatible allow
