import React from 'react';
import { HighlightSpanKind, Token } from 'typescript';

export function textField(placeholder: string, value: string, onChange: (event: React.ChangeEvent<HTMLInputElement>) => void) {
    const id = "text-field-" + placeholder;
    return (
        <input className="form-control" type="text" id={id} value={value} onChange={onChange} />
    )
}

export function checkbox(name: string, checked: boolean, onChange: (event: React.ChangeEvent<HTMLInputElement>) => void) {
    const id = "checkbox-" + name;
    return (
        <div className="form-check form-check-inline">
            <input className="form-check-input" type="checkbox" id={id} value={name} checked={checked} onChange={onChange} />
            <label className="form-check-label" htmlFor={id}>{name}</label>
        </div>
    )
}

export function makeProbabilityBar(style: string, probs: Array<number>, styles: Array<string>, icons?: Array<string>) {
    return (
        <div className={"flex-grow-1 " + style}>
            {probs.map((prob, index) => {
                const pct = prob * 100;
                const pctString = pct.toFixed(0) + "%";
                return (
                    <div key={index} className={styles[index] + " text-nowrap"} style={{ width: pctString }}>
                        <span className="ms-1">{icons ? icons[index] + " " : ""}{pctString}</span>
                    </div>
                );
            })}
        </div>
    )
}

export interface Rgba {
    r: number, g: number, b: number, a: number
}
// object Rgba {
//   def transparent = Rgba(255, 255, 255, 0.0)
// }

function addColors(c1: Rgba, c2: Rgba): Rgba {
    if (c1.a == 0.0) { return c2 } else if (c2.a == 0.0) { return c1 } else {
        const alpha = 1.0 - ((1.0 - c1.a) * (1.0 - c2.a))
        return {
            r: (c1.a * c1.r / alpha) + ((1.0 - c1.a) * (c2.r * c2.a) / alpha),
            g: (c1.a * c1.g / alpha) + ((1.0 - c1.a) * (c2.g * c2.a) / alpha),
            b: (c1.a * c1.b / alpha) + ((1.0 - c1.a) * (c2.b * c2.a) / alpha),
            a: alpha
        }
    }
}

function sigmoid(x: number): number {
    return 1.0 / (1.0 + Math.exp(-x));
}

function logit(x: number): number {
    return Math.log(x / (1.0 - x));
}

export function makeIntoHighlight(color: Rgba) {
    return {
        r: sigmoid(logit(color.r / 255.0) - 1) * 255,
        g: sigmoid(logit(color.g / 255.0) - 1) * 255,
        b: sigmoid(logit(color.b / 255.0) - 1) * 255,
        a: 0.5
    }
    // return {
    //     r: 255 - ((255 - color.r) * 3),
    //     g: 255 - ((255 - color.g) * 3),
    //     b: 255 - ((255 - color.b) * 3),
    //     a: 0.33
    // }
}

function getColorStyleString(color: Rgba): string {
    return "rgba(" + Math.round(color.r) + "," + Math.round(color.g) + "," + Math.round(color.b) + "," + color.a.toFixed(4) + ")";
}
const transparent = { r: 0, g: 0, b: 0, a: 0 }

//   def toColorStyleString = f"rgba(${scala.math.round(r)}%d, ${scala.math.round(g)}%d, ${scala.math.round(b)}%d, $a%.4f)"



const noSpaceBefore = [
    ".",
    ",",
    "!",
    "?",
    ";",
    ":",
    "''",
    "n't",
    "'s",
    "'re",
    "'ve",
    "'ll",
    "na",
    "'m",
    "'d",
    "%",
    "-",
    "+",
    "-RRB-",
    "-RCB-",
    "-RSB-",
    ")",
    "]",
    "}",
    "/.",
    "/?",
    "°"
]

const noSpaceAfter = [
    "``",
    "$",
    "£",
    "€",
    "#",
    "-",
    "-LRB-",
    "-LCB-",
    "-LSB-",
    "(",
    "[",
    "{"
]

function normalizeToken(token: string) {
    if (token === "`") return "'"
    else if (token === "``") return "\""
    else if (token === "''") return "\""
    else if (token === "-LRB-") return "("
    else if (token === "-RRB-") return ")"
    else if (token === "-LCB-") return "{"
    else if (token === "-RCB-") return "}"
    else if (token === "-LSB-") return "["
    else if (token === "-RSB-") return "]"
    else if (token === "/.") return "."
    else if (token === "/?") return "?"
    else if (token === "/-") return "-"
    else if (token === "--") return "-"
    else return token.replaceAll("\\\\/", "/");
}

export function renderSpan(
    story: Array<String>,
    span: [number, number]
) {
    var res = "";
    const end = Math.min(span[1], story.length);
    for (var i = span[0]; i < end; i++) {
        if (!(i === span[0] || noSpaceBefore.includes(story[i].toString()) || noSpaceAfter.includes(story[i - 1].toString()) || story[i].startsWith("'"))) {
            res += " ";
        }
        res += normalizeToken(story[i].toString());
    }
    return res;
}

interface TokenHolder {
    token: string;
    highlights: Array<string>;
    prevSpaceHighlights: Array<string>;
}

export function renderStoryAsHtml(
    story: Array<String>,
    highlightSpans: Array<[[number, number], string]>,
    highlightColors: Record<string, Rgba>,
    quotesOnly: boolean
) {
    // establish the layers of highlighting to apply at each token
    const storyTokens: Array<TokenHolder> = story.map((token) => { return { token: token.toString(), highlights: [], prevSpaceHighlights: [] } });
    highlightSpans.forEach((spanPair) => {
        const span = spanPair[0];
        const style = spanPair[1];
        // we shouldn't have had any out-of-bounds spans, but we had someone enter the OOB span indices _manually_
        // to show they were at the end of the story! That's actually pretty awesome strategy lol
        const end = Math.min(span[1], storyTokens.length);
        for (var i = span[0]; i < end; i++) {
            storyTokens[i].highlights.push(style);
            if (i > span[0]) {
                storyTokens[i].prevSpaceHighlights.push(style);
            }
        }
    })

    const elts: Array<React.ReactElement<any, any>> = [];
    var curElts: Array<React.ReactElement<any, any>> = [];
    var curStr: string = "";
    var curColorStack: Array<string> = [];
    // TODO: add quote indices for quotes panel
    var curSpanBegin: number = -1;

    function processToken(token: string, index: number, highlights: Array<string>) {
        if (JSON.stringify(highlights) !== JSON.stringify(curColorStack)) {
            console.log("highlight change");
            console.log(curColorStack);
            if (curStr.length > 0) {
                curElts.push(<span>{curStr}</span>);
                curStr = "";
            }
            // todo color the span properly
            const color = curColorStack.length > 0 ? curColorStack.reduce((acc, cur) => addColors(acc, highlightColors[cur] || transparent), transparent) : undefined
            const styleAttr = color ? { backgroundColor: getColorStyleString(color) } : {};
            if (!quotesOnly || curColorStack.length > 0) {
                elts.push(<span style={styleAttr}> {curElts}</span >);
                if (quotesOnly && highlights.length === 0 && curSpanBegin > -1) {
                    elts.push(<span className="text-faded"> ({curSpanBegin}-{index})</span>);
                    curSpanBegin = -1
                }
            } else if (elts.length > 0) {
                if (curElts.length > 1) {
                    elts.push(<span><br />...<br /></span >);
                } else {
                    elts.push(<span> ... </span >);
                }
            }

            curElts = [];

            if (curSpanBegin === -1 && highlights.length > 0) {
                curSpanBegin = index;
            }
            curColorStack = highlights;
        }
        if (token === "\n") {
            console.log("newline");
            if (curStr.length > 0) {
                curElts.push(<span>{curStr}</span>);
                curStr = "";
            }
            curElts.push(<br />);
        } else {
            curStr += normalizeToken(token);
        }
    }

    for (var i = 0; i < story.length; i++) {
        const curToken = storyTokens[i]
        const prevToken = storyTokens[i - 1]
        // potentially add a space before the current string
        if (!(i === 0 || noSpaceBefore.includes(curToken.token) || noSpaceAfter.includes(prevToken.token) || curToken.token.startsWith("'"))) {
            processToken(" ", i, curToken.prevSpaceHighlights);
        }
        processToken(curToken.token, i, curToken.highlights);
    }

    if (curStr.length > 0) {
        curElts.push(<span>{curStr}</span>);
        curStr = "";
    }
    // todo color the span properly
    const color = curColorStack.length > 0 ? curColorStack.reduce((acc, cur) => addColors(acc, highlightColors[cur] || transparent), transparent) : undefined
    const styleAttr = color ? { backgroundColor: getColorStyleString(color) } : {};
    if (!quotesOnly || curColorStack.length > 0) {
        elts.push(<span style={styleAttr}> {curElts}</span >);
        if (quotesOnly && curSpanBegin > -1) {
            elts.push(<span className="text-faded">({curSpanBegin}-{storyTokens.length})</span>);
            curSpanBegin = -1
        }
    }

    return <span>{elts}</span>;
}