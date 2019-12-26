# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Added
- Information about early/late night commits
- Information about commits on working days / weekend
### Changed
### Removed
### Fixed

## [0.4.0] - 2019-12-22
### Added
- Pie chart for commit statistics (self-committed commits vs. non self-committed commits)
- Commit counts for contributors statistic
- Contributor rankings based on commit counts
- Timeline chart for authored and committed commits
- File change statistics
- Information about comitter/author emails and corresponding date information for commit
- Log output using logback
- Information about commit message length
### Changed
- Rework commit lists, now displayed as table
- Rework contributor list, now displayed as table
- Authored and committed count is added to separate categories
- Parallelized repo analysis steps

## [0.3.0] - 2019-10-29
### Added
- Information about contributors
- Information about number of commits
- Information about self-committed commits
- Metainformation about analysis
- Information about commits with different author/committer
- Printouts for analysis steps
### Changed
- Include bootstrap for HTML output
- Commit information moved to contributors section
### Removed
- Separate sections in HTML report about commits by author and committer

## [0.2.0] - 2019-10-24
### Added
- HTML output for analysis

## [0.1.0] - 2019-10-24
### Added
- Analyse commits by author and committer

[Unreleased]: https://github.com/gernd/repo-analyzer/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/gernd/repo-analyzer/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/gernd/repo-analyzer/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/gernd/repo-analyzer/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/gernd/repo-analyzer/compare/v0.0.1...v0.1.0
