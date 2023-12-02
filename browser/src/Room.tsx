import React from 'react';
import { useEffect, useState } from 'react';
import { Link, LoaderFunction } from "react-router-dom";
import './Room.css';

import { Form, useLoaderData } from "react-router-dom";

// Could do the async call here instead.
// Meh. I don't really care atm. it works
export const loader: LoaderFunction = ({ params }) => {
    return { roomName: params.roomName };
};

interface Roles {
    Judge?: string;
    "Debater A"?: string;
    "Debater B"?: string;
}

interface DebateSetup {
    //   rules: DebateRules,
    //   sourceMaterial: SourceMaterial,
    question: String;
    answers: Array<String>;
    correctAnswerIndex: number;
    roles: Roles;
    //   offlineJudges: Map[String, Option[OfflineJudgingMode]],
    creationTime: bigint;
}

// interface DebateRound {

// }

interface Debate {
    setup: DebateSetup,
    // rounds: Array<DebateRound>,
    //   feedback: Map[String, Feedback.SurveyResponse],
    //   scratchpads: Map[DebateRole, Vector[Vector[SpeechSegment]]] = Map()
}

function DebateDisplay({ roomName, debate }: { roomName: string; debate: Debate }) {
    return (
        <div className="container">
            <Link to="/debate">Back</Link>
            {debate.setup.question}
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
