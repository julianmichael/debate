import React from 'react';
import { useEffect, useState } from 'react';
import './App.css';

import { TransitionGroup } from 'react-transition-group' // ES6

// var CSSTransitionGroup = require('react-transition-group/CSSTransitionGroup') // ES5 with npm

function textField(placeholder: string, value: string, onChange: (event: React.ChangeEvent<HTMLInputElement>) => void) {
  var id = "text-field-" + placeholder;
  return (
    <input className="form-control" type="text" id={id} value={value} onChange={onChange} />
  )
}

function checkbox(name: string, checked: boolean, onChange: (event: React.ChangeEvent<HTMLInputElement>) => void) {
  var id = "checkbox-" + name;
  return (
    <div className="form-check form-check-inline">
      <input className="form-check-input" type="checkbox" id={id} value={name} checked={checked} onChange={onChange} />
      <label className="form-check-label" htmlFor={id}>{name}</label>
    </div>
  )
}

interface Filters {
  human: boolean;
  ai: boolean;
  debate: boolean;
  consultancy: boolean;
  judge: boolean;
  honest: boolean;
  dishonest: boolean;
  correct: boolean;
  incorrect: boolean;
  tied: boolean;
  unfinished: boolean;
  noLiveJudge: boolean;
  included: boolean;
  excluded: boolean;
  searchQuery: string;
}

interface DebateSetting {
  isHuman: boolean;
  isDebate: boolean;
}

interface UltimateRoles {
  Judge?: string;
  "Honest Debater"?: string;
  "Dishonest Debater"?: string;
}

interface JudgingResult {
  correctAnswerIndex: number,
  // numContinues: Int,
  finalJudgement: Array<number>,
  // judgeReward: Double
}

interface DebateResult {
  // timestamp: Long,
  // correctAnswerIndex: bigint,
  // endedBy: DebateEndReason,
  judgingInfo?: JudgingResult
}

interface RoomStatus {
  WaitingToBegin?: Record<string, never>;
  InProgress?: Record<string, never>;
  Complete?: {
    result: DebateResult;
  };
}

// TODO: status should hold outcome info
interface DebateMetadata {
  setting: DebateSetting;
  name: String;
  // sourceMaterialId: SourceMaterialId;
  storyTitle: String;
  question: String;
  // roleAssignments: RoleAssignments;
  ultimateRoles: UltimateRoles;
  offlineJudges: Record<string, string>;
  creationTime: bigint;
  status: RoomStatus;
  latestUpdateTime: bigint;
  // currentSpeakers: Set[LiveDebateRole];
  // currentParticipants: Set[String];
  includedInPaper: Boolean;
}

function getSettingString(setting: DebateSetting) {
  var res = "";
  if (setting.isHuman) {
    res += "Human ";
  } else {
    res += "AI ";
  }
  if (setting.isDebate) {
    res += "Debate";
  } else {
    res += "Consultancy";
  }
  return res;
}

function getRoleLabelClass(role: string) {
  if (role === "Judge") {
    return "label-judge me-2";
  } else if (role === "Honest Debater") {
    return "label-honest-debater me-2";
  } else if (role === "Dishonest Debater") {
    return "label-dishonest-debater me-2";
  } else {
    return "";
  }
}

function shortenName(name: string) {
  // cut off at the first space
  var spaceIndex = name.indexOf(" ");
  if (spaceIndex === -1) {
    return name;
  } else {
    return name.substring(0, spaceIndex);
  }
}

function getStatusLabel(status: RoomStatus) {
  var judgingInfo = status.Complete?.result.judgingInfo
  var judgment = judgingInfo?.finalJudgement
  var probCorrect = judgment?.[(judgingInfo as JudgingResult).correctAnswerIndex];
  if (probCorrect === undefined) {
    if (status.Complete === undefined) {
      return "unfinished"
    } else {
      return "no live judge"
    }
  } else {
    if (probCorrect > 0.5) {
      return "correct"
    } else if (probCorrect < 0.5) {
      return "incorrect"
    } else {
      return "tied"
    }
  }
}

function makeProbabilityBar(probCorrect: number) {
  var pctCorrect = probCorrect * 100;
  var pctCorrectString = pctCorrect.toFixed(0) + "%";
  var pctIncorrect = (1.0 - probCorrect) * 100;
  var pctIncorrectString = pctIncorrect.toFixed(0) + "%";
  return (
    <div className="judgment-bar">
      <div className="judgment-bar-correct" style={{ width: pctCorrectString }}>
        <span className="ms-1">{pctCorrectString}</span>
      </div>
      <div className="judgment-bar-incorrect" style={{ width: pctIncorrectString }}>
        <span className="ms-1">{pctIncorrectString}</span>
      </div>
    </div>
  )
}

function getStatusDisplay(status: RoomStatus) {
  var judgingInfo = status.Complete?.result.judgingInfo
  var judgment = judgingInfo?.finalJudgement
  var probCorrect = judgment?.[(judgingInfo as JudgingResult).correctAnswerIndex];
  if (probCorrect === undefined) {
    if (status.Complete === undefined) {
      return (<span className="unfinished-label">unfinished</span>)
    } else {
      return (<span className="no-live-judge-label">no live judge</span>)
    }
  } else {
    return makeProbabilityBar(probCorrect);
  }
}

function includesIgnoreCase(str: string, substr: string) {
  return str.toLowerCase().includes(substr.toLowerCase());
}

// TODO filter on outcomes
function includeDebate(filters: Filters, debate: DebateMetadata) {
  // return (filters.human || !debate.setting.isHuman)
  var statusLabel = getStatusLabel(debate.status);
  return (
    (filters.human || !debate.setting.isHuman) &&
    (filters.ai || debate.setting.isHuman) &&
    (filters.debate || !debate.setting.isDebate) &&
    (filters.consultancy || debate.setting.isDebate) &&
    (filters.judge || debate.ultimateRoles.Judge === undefined) &&
    (filters.honest || debate.ultimateRoles['Honest Debater'] === undefined) &&
    (filters.dishonest || debate.ultimateRoles['Dishonest Debater'] === undefined) &&
    (filters.correct || !(statusLabel === "correct")) &&
    (filters.incorrect || !(statusLabel === "incorrect")) &&
    (filters.unfinished || !(statusLabel === "unfinished")) &&
    (filters.noLiveJudge || !(statusLabel === "no live judge")) &&
    (filters.tied || !(statusLabel === "tied")) &&
    (filters.included || !debate.includedInPaper) &&
    (filters.excluded || debate.includedInPaper) &&
    filters.searchQuery.split("\\s+").every((word) => {
      return (
        includesIgnoreCase(debate.name.toString(), word) ||
        includesIgnoreCase(debate.storyTitle.toString(), word) ||
        includesIgnoreCase(debate.question.toString(), word) ||
        includesIgnoreCase(Object(debate.ultimateRoles).Judge?.toString() || "", word) ||
        includesIgnoreCase(Object(debate.ultimateRoles)["Honest Debater"]?.toString() || "", word) ||
        includesIgnoreCase(Object(debate.ultimateRoles)["Dishonest Debater"]?.toString() || "", word)

      )
    })
  )
}

function numLabel(label: string, num: number) {
  if (num === 1) {
    return "1 " + label;
  } else {
    return num + " " + label + "s";
  }
}

function roleLabel(role: string, debate: DebateMetadata) {

  if (Object(debate.ultimateRoles)[role] === undefined) {
    return null;
  } else {
    return (
      <span key={role} className={getRoleLabelClass(role)}>{shortenName(Object(debate.ultimateRoles)[role])}</span>
    );
  }
}

function LoadedDebatesTable(filters: Filters, debates: DebateMetadata[]) {
  var includedDebates = debates.filter((debate) => includeDebate(filters, debate));
  return (
    <div>
      Showing {numLabel("session", includedDebates.length)}.
      <table className="table">
        <thead><tr>
          <th scope="col">Room Name</th>
          <th scope="col">Setting</th>
          <th scope="col"></th>
          <th scope="col">Outcome</th>
          <th scope="col">Judge</th>
          <th scope="col">Honest</th>
          <th scope="col">Dishonest</th>
          {/* <th scope="col">
            Participants
            (<span className="label-judge">Judge</span>, <span className="label-honest-debater">Honest</span>, <span className="label-dishonest-debater">Dishonest</span>)
          </th> */}
        </tr></thead>
        <tbody>
          {/* <TransitionGroup
          transitionName="table-row-anim"
          transitionEnterTimeout={500}
          transitionLeaveTimeout={300}> */}
          {includedDebates.map((debate) => {
            return (<tr key={debate.name.toString()}>
              <td>{debate.name}</td>
              <td>{debate.setting.isHuman ? "Human" : "AI"}</td>
              <td>{debate.setting.isDebate ? "Debate" : "Consultancy"}</td>
              <td className="status-display-cell">{getStatusDisplay(debate.status)}</td>
              <td>{roleLabel("Judge", debate)}</td>
              <td>{roleLabel("Honest Debater", debate)}</td>
              <td>{roleLabel("Dishonest Debater", debate)}</td>
              {/* <td>
                {['Judge', 'Honest Debater', 'Dishonest Debater'].map((role) => {
                  if (Object(debate.ultimateRoles)[role] === undefined) {
                    return null;
                  } else
                    return (<span key={role} className={getRoleLabelClass(role)}>{shortenName(Object(debate.ultimateRoles)[role])}</span>)
                })}
              </td> */}
            </tr>)
          })}
          {/* </TransitionGroup> */}
        </tbody>
      </table >
    </div>
  )
}

function DebatesTable(filters: Filters) {
  const [metadata, setMetadata] = useState<DebateMetadata[]>([]);
  const [error, setError] = useState<Error | null>(null);
  useEffect(() => {
    fetch("/debate/debates-metadata.json")
      .then(res => res.json())
      .then(
        (result) => {
          setMetadata(result);
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
    return <div>Error: {error.message}</div>;
  } else if (metadata.length === 0) {
    return <div>Loading debate metadata...</div>;
  } else {
    return LoadedDebatesTable(filters, metadata);
  }
}

function App() {
  // hooks for filter state

  // human, AI, debate, and consultancy checkbox state hooks
  const [human, setHuman] = React.useState(true);
  const [ai, setAI] = React.useState(true);
  const [debate, setDebate] = React.useState(true);
  const [consultancy, setConsultancy] = React.useState(true);

  // roles
  const [judge, setJudge] = React.useState(true);
  const [honest, setHonest] = React.useState(true);
  const [dishonest, setDishonest] = React.useState(true);

  // correct / incorrect checkbox state hooks
  const [correct, setCorrect] = React.useState(true);
  const [incorrect, setIncorrect] = React.useState(true);
  const [tied, setTied] = React.useState(true);
  const [unfinished, setUnfinished] = React.useState(false);
  const [noLiveJudge, setNoLiveJudge] = React.useState(false);

  // included in paper analysis
  const [included, setIncluded] = React.useState(true);
  const [excluded, setExcluded] = React.useState(false);

  // search terms hook
  const [searchQuery, setSearchQuery] = React.useState("");

  return (
    <div className="container">
      <header>
        <h1>Debate Helps Supervise Unreliable Experts</h1>
        <p>Data browser for the <a href="https://arxiv.org/abs/2311.08702">paper</a> of the above title.
          More information about the project can be found <a href="https://github.com/julianmichael/debate">here</a>.
        </p>
        <h4>Filters</h4>
        <div className="row mb-2">
          <div className="col-sm-2">
            Setting:
          </div>
          <div className="col">
            {checkbox("Human", human, (event) => setHuman(event.target.checked))}
            {checkbox("AI", ai, (event) => setAI(event.target.checked))}
            {checkbox("Debate", debate, (event) => setDebate(event.target.checked))}
            {checkbox("Consultancy", consultancy, (event) => setConsultancy(event.target.checked))}
          </div>
        </div>
        <div className="row mb-2">
          <div className="col-sm-2">
            Outcome:
          </div>
          <div className="col">
            {checkbox("Correct", correct, (event) => setCorrect(event.target.checked))}
            {checkbox("Incorrect", incorrect, (event) => setIncorrect(event.target.checked))}
            {checkbox("Tied", tied, (event) => setTied(event.target.checked))}
            {checkbox("Unfinished", unfinished, (event) => setUnfinished(event.target.checked))}
            {checkbox("No live judge", noLiveJudge, (event) => setNoLiveJudge(event.target.checked))}
          </div>
        </div>
        <div className="row mb-2">
          <div className="col-sm-2">
            Roles:
          </div>
          <div className="col">
            {checkbox("Judge", judge, (event) => setJudge(event.target.checked))}
            {checkbox("Honest", honest, (event) => setHonest(event.target.checked))}
            {checkbox("Dishonest", dishonest, (event) => setDishonest(event.target.checked))}
          </div>
        </div>
        <div className="row mb-2">
          <div className="col-sm-2">
            Used in the paper:
          </div>
          <div className="col">
            {checkbox("Yes", included, (event) => setIncluded(event.target.checked))}
            {checkbox("No", excluded, (event) => setExcluded(event.target.checked))}
          </div>
        </div>
        <div className="row mb-2">
          <div className="col-sm-2">
            Search terms:
          </div>
          <div className="col">
            {textField("Enter keywords", searchQuery, (event) => setSearchQuery(event.target.value))}
          </div>
        </div>
      </header>
      <DebatesTable
        human={human} ai={ai}
        debate={debate} consultancy={consultancy}
        judge={judge} honest={honest} dishonest={dishonest}
        correct={correct} incorrect={incorrect} unfinished={unfinished} noLiveJudge={noLiveJudge} tied={tied}
        included={included} excluded={excluded} searchQuery={searchQuery} />
    </div>
  );
}

export default App;
