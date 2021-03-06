# Changelog

## [0.3.3] - 2022-01-04

**Attention!**
Lichess updated their [Fair Play Guidelines](https://lichess.org/page/fair-play). Keyboard extensions [are now banned](https://lichess.org/page/play-extensions) [😭](#noinvert).
Please comply with Lichess rules and don't use keyboard input when competing against other players.

Changes:
- Updated tutorial to instruct not to use OCES in games.


## [0.3.2] - 2021-11-29 
> [Version ignored in changelog within extension](#ignore-in-changelog-within-extension)

- Fix the tutorial link on the preferences page.
- Hide changelog if no changes require being acknowledged by the user.


## [0.3.1] - 2021-11-28

- Fix extension getting stuck after a missclick on an unreachable square.


## [0.3] - 2021-11-28

**Breaking changes:**
- From now on, after a missclick you will need to confirm the move, even when only one piece is able to make the move to the selected square.
This behavior can be customized in the extension preferences.
**The old behavior with automatically performing the only move is still possible, turn it on in the extension preferences.**
It's also possible to ignore missclicks altogether.

Other changes:
- Tutorial and preferences can handle browser preference for dark mode.
- Relevant changes are shown after update.


## [0.2.1] - 2021-11-23

- Fixed first rank of position not being detected in Firefox.
- Fixed some typos in the tutorial.


## [0.2] - 2021-11-22

- Replaced browser_action with page_action activated only on lichess.org
- Showing an info banner after the configuration is saved
- Added a tutorial which is opened after installation


## [0.1.2] - 2021-11-20

Fixed lichess key events being shadowed by the extension controls.


## [0.1.1] - 2021-11-19

Initial version.

Features:
- moving pieces
- disambiguation
- handling missclicks
- keymap customization


[0.3.3]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.3.3
[0.3.2]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.3.2
[0.3.1]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.3.1
[0.3]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.3
[0.2.1]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.2.1
[0.2]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.2
[0.1.2]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.1.2
[0.1.1]: https://github.com/klausweiss/online-chess-enhancement-suite/releases/tag/0.1.1
