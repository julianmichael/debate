import React from 'react';
import { useEffect, useState } from 'react';
import { Link, LoaderFunction } from "react-router-dom";
import './Room.css';

import { Form, useLoaderData } from "react-router-dom";
import { ArrowLeft } from 'react-bootstrap-icons';

import { checkbox, makeProbabilityBar, renderSpan, renderStoryAsHtml, Rgba, makeIntoHighlight } from './Utils';

const noSpaceBefore = ['.', ',', '!', '?', ':', ';', ')', ']', '}', '”', '’', '…', "n't"];
const noSpaceAfter = ['(', '[', '{', '“', '‘'];

function Story({ story, spans, highlightColors }: { story: Array<String>; spans: Array<[[number, number], string]>; highlightColors: Record<string, Rgba> }) {
    console.log("story length: " + story.length);
    console.log("spans: " + JSON.stringify(spans));
    console.log("highlightColors: " + JSON.stringify(highlightColors));
    return (
        <div>
            <div className="card mb-2">
                <div className="card-header">
                    <span className="card-title">All Quotes</span>
                </div>
                <div className="card-body">
                    {renderStoryAsHtml(story, spans, highlightColors, true)}
                </div>
            </div>
            <div className="card">
                <div className="card-header">
                    <span className="card-title">Story</span>
                </div>
                <div className="card-body">
                    {renderStoryAsHtml(story, spans, highlightColors, false)}
                </div>
            </div>
        </div>
    );
}

const time = new Intl.DateTimeFormat('en-US', { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })

// Could do the async call here instead.
// Meh. I don't really care atm. it works
export const loader: LoaderFunction = ({ params }) => {
    return { roomName: params.roomName };
};

interface SourceMaterial {
    QuALITYSourceMaterial?: {
        articleId: string;
        title: string;
        contents: Array<String>;
    };
    CustomSourceMaterial?: {
        title: string,
        contents: Array<string>
    };
}

function getTitle(sourceMaterial: SourceMaterial) {
    if (sourceMaterial.QuALITYSourceMaterial !== undefined) {
        return sourceMaterial.QuALITYSourceMaterial.title;
    } else if (sourceMaterial.CustomSourceMaterial !== undefined) {
        return sourceMaterial.CustomSourceMaterial.title;
    } else {
        return "<no title>";
    }
}

function getContents(sourceMaterial: SourceMaterial) {
    if (sourceMaterial.QuALITYSourceMaterial !== undefined) {
        return sourceMaterial.QuALITYSourceMaterial.contents;
    } else if (sourceMaterial.CustomSourceMaterial !== undefined) {
        return sourceMaterial.CustomSourceMaterial.contents;
    } else {
        return [];
    }
}

interface Roles {
    Judge?: string;
    "Debater A"?: string;
    "Debater B"?: string;
}

interface DebateSetup {
    //   rules: DebateRules,
    sourceMaterial: SourceMaterial,
    question: String;
    answers: Array<String>;
    correctAnswerIndex: number;
    roles: Roles;
    //   offlineJudges: Map[String, Option[OfflineJudgingMode]],
    creationTime: bigint;
}


interface SpeechSegment {
    Text?: { text: string; };
    Quote?: { span: [number, number]; };
}

interface DebateSpeech {
    speaker: string;
    timestamp?: number;
    content: Array<SpeechSegment>;
}

interface OfflineJudgment {
    mode: string;
    startTimeMillis: bigint;
    judgments: Array<{
        distribution: Array<number>;
        feedback: DebateSpeech;
        endDebate: Boolean;
    }>;
}

interface DebateRound {
    SimultaneousSpeeches?: { speeches: Record<number, DebateSpeech>; };
    SequentialSpeeches?: { speeches: Record<number, DebateSpeech>; };
    JudgeFeedback?: {
        distribution: Array<number>;
        feedback: DebateSpeech;
        endDebate: Boolean;
    };
    NegotiateEnd?: Record<number, boolean>;
    OfflineJudgments?: Record<string, OfflineJudgment>;
}

interface Debate {
    setup: DebateSetup,
    rounds: Array<DebateRound>,
    //   feedback: Map[String, Feedback.SurveyResponse],
    //   scratchpads: Map[DebateRole, Vector[Vector[SpeechSegment]]] = Map()
}

interface JudgmentInfo {
    correctAnswerIndex: number;
    distribution: Array<number>;
    endDebate: Boolean;
    styles: Array<string>;
    icons: Array<string>;
}

function renderJudgment(
    judgment: JudgmentInfo,
    showCorrectAnswer: boolean
) {
    const label = judgment.endDebate ? (<b className="me-1">Final judgment:</b>) : (<b className="me-1">Current beliefs:</b>);
    var chosenIndex = -1;
    judgment.distribution.forEach((prob, index) => {
        if (prob > 0.5) {
            chosenIndex = index;
        }
    });
    const style = judgment.endDebate ? (showCorrectAnswer ? (chosenIndex === judgment.correctAnswerIndex ? "judgment-bar-final-correct" : "judgment-bar-final-incorrect") : "judgment-bar-final") : "judgment-bar-large";
    return (<div className="d-flex mt-1">{label}{makeProbabilityBar(style, judgment.distribution, judgment.styles, judgment.icons)}</div>)
}

function renderSpeech(
    story: Array<String>,
    classes: string,
    icon: string,
    speech: DebateSpeech,
    showCorrectAnswer: boolean,
    judgment?: JudgmentInfo
) {
    return (
        <div className={"speech-box " + classes}>
            <div className="d-flex justify-content-between">
                <div>
                    <b>{icon} {speech.speaker}</b>
                </div>
                <div className="text-faded">
                    {speech.timestamp ? time.format(speech.timestamp) : ""}
                </div>
            </div>
            <hr />
            {speech.content.map((segment, index) => {
                if (segment.Text !== undefined) {
                    return (
                        <span key={index}>{segment.Text.text}</span>
                    );
                } else if (segment.Quote !== undefined) {
                    return (
                        <React.Fragment key={index}>
                            <span className="quote">{renderSpan(story, segment.Quote.span)}</span>
                            <span className="text-faded"> ({segment.Quote.span[0]}-{segment.Quote.span[1]})</span>
                        </React.Fragment>
                    );
                } else {
                    console.log("Unknown segment type: " + JSON.stringify(segment));
                    return (
                        <span key={index}></span>
                    );
                }
            })}
            {judgment !== undefined ? (renderJudgment(judgment, showCorrectAnswer)) : (<></>)}
        </div>
    );
}

function DebateDisplay({ roomName, debate }: { roomName: string; debate: Debate }) {
    const [showOfflineJudgments, setShowOfflineJudgments] = useState<boolean>(false);
    const [showCorrectAnswer, setShowCorrectAnswer] = useState<boolean>(false);

    const aStyle = showCorrectAnswer ? (debate.setup.correctAnswerIndex === 0 ? "correct" : "incorrect") : "debaterA";
    const bStyle = showCorrectAnswer ? (debate.setup.correctAnswerIndex === 1 ? "correct" : "incorrect") : "debaterB";
    const judgmentStyles = [aStyle, bStyle]

    const judgeIcon = "👩‍⚖️";
    const aIcon = showCorrectAnswer ? (debate.setup.correctAnswerIndex === 0 ? "😇" : "😈") : "A.";
    const bIcon = showCorrectAnswer ? (debate.setup.correctAnswerIndex === 1 ? "😇" : "😈") : "B.";
    const debaterIcons = [aIcon, bIcon];

    const isAI = debate.setup.roles['Debater A'] === 'GPT-4' || debate.setup.roles['Debater B'] === 'GPT-4';
    const isDebate = debate.setup.roles['Debater A'] !== undefined && debate.setup.roles['Debater B'] !== undefined;
    const setting = (isAI ? 'AI' : 'Human') + ' ' + (isDebate ? 'Debate' : 'Consultancy');

    const story = getContents(debate.setup.sourceMaterial);
    const allHighlights = new Array<[[number, number], string]>();
    debate.rounds.forEach((round) => {
        if (round.SimultaneousSpeeches !== undefined) {
            for (const [speaker, speech] of Object.entries(round.SimultaneousSpeeches.speeches)) {
                const speakerLabel = speaker === "0" ? "Debater A" : "Debater B";
                speech.content.forEach((segment) => {
                    if (segment.Quote !== undefined) {
                        allHighlights.push([segment.Quote.span, speakerLabel]);
                    }
                });
            }
        }
        if (round.SequentialSpeeches !== undefined) {
            for (const [speaker, speech] of Object.entries(round.SequentialSpeeches.speeches)) {
                const speakerLabel = speaker === "0" ? "Debater A" : "Debater B";
                speech.content.forEach((segment) => {
                    if (segment.Quote !== undefined) {
                        allHighlights.push([segment.Quote.span, speakerLabel]);
                    }
                });
            }
        }
    })
    const correctHighlightColor = makeIntoHighlight({ r: 240, g: 255, b: 235, a: 0.9 });
    const incorrectHighlightColor = makeIntoHighlight({ r: 255, g: 230, b: 225, a: 0.9 });
    const debaterAHighlightColor = makeIntoHighlight({ r: 240, g: 248, b: 255, a: 1.0 });
    const debaterBHighlightColor = makeIntoHighlight({ r: 250, g: 230, b: 210, a: 1.0 });
    const highlightColors = {
        "Debater A": showCorrectAnswer ? (debate.setup.correctAnswerIndex === 0 ? correctHighlightColor : incorrectHighlightColor) : debaterAHighlightColor,
        "Debater B": showCorrectAnswer ? (debate.setup.correctAnswerIndex === 1 ? correctHighlightColor : incorrectHighlightColor) : debaterBHighlightColor
    }

    return (
        <div className="container">
            <div className="d-flex align-items-center flex-wrap my-2">
                <div>
                    <Link to="/debate"><button className="btn btn-sm"><ArrowLeft /> Back</button></Link>
                </div>
                <div className="ms-2">
                    <b>Room:</b> {roomName}
                </div>
                <div className="ms-2">
                    <b>Setting:</b> {setting}
                </div>
                {/* <div className="ms-2">
                    {checkbox("Show offline judgments", showOfflineJudgments, (event) => setShowOfflineJudgments(event.target.checked))}
                </div> */}
                <div className="ms-2">
                    {checkbox("Show correct answer", showCorrectAnswer, (event) => setShowCorrectAnswer(event.target.checked))}
                </div>
            </div>
            <div className="card mb-2 custom-card">
                <div className="card-header">
                    <span className="card-title">Setup (preset)</span>
                </div>
                <div className="card-body">
                    <div> <b>Story:</b> {getTitle(debate.setup.sourceMaterial)} </div>
                    <div> <b>Question:</b> {debate.setup.question} </div>
                    <div className="d-flex">
                        {renderSpeech(story, aStyle + " speech-box-left me-1", aIcon, { speaker: (debate.setup.roles['Debater A'] || "").toString(), content: [{ Text: { text: debate.setup.answers[0].toString() } }] }, showCorrectAnswer)}
                        {renderSpeech(story, bStyle + " speech-box-right ms-1", bIcon, { speaker: (debate.setup.roles['Debater B'] || "").toString(), content: [{ Text: { text: debate.setup.answers[1].toString() } }] }, showCorrectAnswer)}
                    </div>
                </div>
            </div>
            <div className="mb-2">
                {debate.rounds.map((round, index) => {
                    if (round.JudgeFeedback !== undefined) {
                        return (<div key={index}>{renderSpeech(story, "judge", judgeIcon, round.JudgeFeedback.feedback, showCorrectAnswer, {
                            correctAnswerIndex: debate.setup.correctAnswerIndex, distribution: round.JudgeFeedback.distribution, endDebate: round.JudgeFeedback.endDebate, styles: judgmentStyles, icons: debaterIcons
                        })}</div>);
                    } else if (round.SimultaneousSpeeches !== undefined) {
                        return (
                            <div key={index} className="d-flex">
                                {renderSpeech(story, aStyle + " speech-box-left me-1", aIcon, round.SimultaneousSpeeches.speeches[0], showCorrectAnswer)}
                                {renderSpeech(story, bStyle + " speech-box-right ms-1", bIcon, round.SimultaneousSpeeches.speeches[1], showCorrectAnswer)}
                            </div>
                        );
                    } else if (round.SequentialSpeeches !== undefined) {
                        return (
                            <div key={index}>
                                {round.SequentialSpeeches.speeches[0] ? renderSpeech(story, aStyle + " speech-box-left", aIcon, round.SequentialSpeeches.speeches[0], showCorrectAnswer) : <></>}
                                {round.SequentialSpeeches.speeches[1] ? renderSpeech(story, bStyle + " speech-box-right speech-box-right-seq", bIcon, round.SequentialSpeeches.speeches[1], showCorrectAnswer) : <></>}
                            </div>
                        );
                    } else if (round.OfflineJudgments !== undefined) {
                        return (<></>); // don't render offline judgments here; we do it round-wise
                    } else if (round.NegotiateEnd !== undefined) {
                        return (<></>);
                    } else {
                        console.log("Unknown round type: " + JSON.stringify(round));
                        return (<></>);
                    }
                })}
            </div>
            <div className="my-2">
                <Story story={story} spans={allHighlights} highlightColors={highlightColors} />
            </div>
        </div>
    );
}

function Room() {
    const { roomName } = useLoaderData() as { roomName: string };
    const [debate, setDebate] = useState<Debate | undefined>(undefined);
    const [error, setError] = useState<Error | null>(null);
    useEffect(() => {
        fetch("/debate/debates/official/" + roomName + ".json")
            .then(res => res.json())
            .then(
                (result) => {
                    setDebate(result);
                },
                // Note: it's important to handle errors here
                // instead of a catch() block so that we don't swallow
                // exceptions from actual bugs in components.
                (error) => {
                    setError(error);
                }
            )
    }, [])

    if (error) {
        return (
            <div className="container">
                <div>
                    <a href="/debate">Back</a>
                    Error: {error.message}
                </div>
            </div>
        );
    } else if (debate === undefined) {
        return (
            <div className="container">
                <div>
                    {/* <button onClick={(event) => exit()}>Back</button> */}
                    <a href="/debate">Back</a>
                    Loading debate...
                </div>
            </div>
        );
    } else {
        return (<DebateDisplay roomName={roomName} debate={debate} />);
    }
}

export default Room;
