# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

## [Unreleased]

## [1.2.1] - 2026-05-22
### Fixed
- Seeded `*rnd*` now yields reproducible worlds across JVM invocations;
  `sort-attrs` pre-sorts attributes by `:name` so generator consumption
  order no longer depends on closure identity hashes in the loader.

## [1.2.0] - 2024-01-22
### Changed
- Updated dependencies.
- Replaced `io.forward/yaml` with `clj-commons/clj-yaml`.
- Trigger CI tests on pull requests.

## [1.1.0] - 2021-01-12
### Added
- Adds the ability to generate DAGs

## [1.0.1] -  2020-02-19
### Added
- Adds the increasing-int generator
- Adds this changelog

## [1.0.0] - 2020-02-19
### Added
- First release
