# Repository Guidelines

## Project Structure & Module Organization
This repository contains the ClojureScript calculator frontend used by the IT-Premium Hugo site. Application source lives in `src/app/`, with the main calculator and proposal flow in `src/app/main.cljs` and benefits rendering in `src/app/benefits.cljs`. Shadow CLJS configuration is in `shadow-cljs.edn`; Node dependencies are declared in `package.json` and `package-lock.json`.

Generated output is written to `prod/`, `target/`, and `.shadow-cljs/`. Do not hand-edit generated JavaScript. The Hugo site consumes the release bundle by copying `prod/main.js` into the static site repository as `static/js/main.js`.

## Build, Test, and Development Commands
- `npm install`: install Node dependencies if `node_modules/` is missing or stale.
- `npx shadow-cljs watch app`: start the development build and dev HTTP server.
- `npx shadow-cljs release app`: build the optimized production bundle in `prod/main.js`.

There is no dedicated automated test suite. Validate calculator changes by running `npx shadow-cljs release app`, then copy the generated `prod/main.js` to the Hugo site and run that site's Hugo build.

## Coding Style & Naming Conventions
Use idiomatic ClojureScript with two-space indentation. Prefer small pure helper functions for pricing rules and keep Reagent components focused on rendering. Preserve existing keyword naming because data arrives from Hugo JSON using snake-case keys such as `:default_selected`, `:has_input`, `:input_name`, and pricing fields from `data/*/quotations.yml`.

## Calculator Data Contract
Service data is owned by the Hugo static site, not this repository. The calculator should support simple `price * count` services and tiered services with `basis`, `default_selected`, `tier_size`, and `multiplier`. Keep package totals and exported proposal totals on the same pricing helper so on-screen prices and generated PDFs do not diverge.

## Commit & Pull Request Guidelines
Keep changes focused: source edits in this repo should be committed separately from Hugo content/data edits unless the release bundle update requires both. Pull requests should mention the affected calculator behavior, the generated `prod/main.js` handoff, and the validation command output.

## Generated Files & Hygiene
Avoid committing `node_modules/`, `.shadow-cljs/`, `target/`, `.DS_Store`, or other local build artifacts. Commit source changes and, when needed by the release workflow, the generated `prod/main.js` or copied Hugo bundle in the target repository according to that repository's process.
