# NYU human debate dataset

In this directory are the debates we gathered in our 2023 debate experiments.
Participant identities are anonymized to silly names chosen with the help of GPT-4.

This data is released under the [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/) license.
It builds on texts available in the public domain from [Project Gutenberg](https://www.gutenberg.org)
as well as the [QuALITY dataset](https://nyu-mll.github.io/quality/) which is also distributed under
CC-BY 4.0.

## Contents

* [debates-metadata.json](debates-metadata.json): A JSON array of metadata for all of the debates we gathered,
  including debates which never finished or weren't included in the final analysis.
  These are also included in a JSON-lines file as [debates-metadata.jsonl].
  They are formatted according to the description under
  [Metadata File Format](#metadata-file-format) below.
* [debates/](debates/): Directory containing all debate data, as readable by the main webapp.
  The debates themselves are contained in [debates/official/], with one debate per file,
  formatted according to the [Debate File Format](#debate-file-format).

## Metadata File Format

The file format for debate room metadata is based in TypeScript and Scala interfaces.
You can find them in two files:
* TypeScript interface at [App.tsx#L68](https://github.com/julianmichael/debate/blob/7c212fbe2540feb736ddff7485cce3c1ab5f9408/browser/src/App.tsx#L68).
* Scala class at [RoomMetadata.scala#L53](https://github.com/julianmichael/debate/blob/7c212fbe2540feb736ddff7485cce3c1ab5f9408/debate/src/RoomMetadata.scala#L53).

## Debate File Format

The file format for debate rooms is based in TypeScript and Scala interfaces.
You can find them in two files:
* TypeScript interface at [Room.tsx#L130](https://github.com/julianmichael/debate/blob/7c212fbe2540feb736ddff7485cce3c1ab5f9408/browser/src/Room.tsx#L130).
* Scala class at [Debate.scala#L21](https://github.com/julianmichael/debate/blob/7c212fbe2540feb736ddff7485cce3c1ab5f9408/debate/src/Debate.scala#L21).