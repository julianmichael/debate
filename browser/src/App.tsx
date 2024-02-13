import React from 'react';
import { useEffect, useState } from 'react';
import { Link } from "react-router-dom";
import './App.css';
import Room from './Room';

import Modal from 'react-bootstrap/Modal';
import { useSessionStorage } from 'usehooks-ts';

import { checkbox, textField, makeProbabilityBar } from './Utils';

// import { TransitionGroup } from 'react-transition-group' // ES6
// const CSSTransitionGroup = require('react-transition-group/CSSTransitionGroup') // ES5 with npm

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
  sortColumn: string;
  setSortColumn: (sortColumn: string) => void;
  sortReversed: boolean;
  setSortReversed: (sortReversed: boolean) => void;
  // setRoom: (room: string) => void;
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
  const spaceIndex = name.indexOf(" ");
  if (spaceIndex === -1) {
    return name;
  } else {
    return name.substring(0, spaceIndex);
  }
}

function getStatusLabel(status: RoomStatus) {
  const judgingInfo = status.Complete?.result.judgingInfo
  const judgment = judgingInfo?.finalJudgement
  const probCorrect = judgment?.[(judgingInfo as JudgingResult).correctAnswerIndex];
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

function getStatusDisplay(status: RoomStatus) {
  const judgingInfo = status.Complete?.result.judgingInfo
  const judgment = judgingInfo?.finalJudgement
  const probCorrect = judgment?.[(judgingInfo as JudgingResult).correctAnswerIndex];
  if (probCorrect === undefined) {
    if (status.Complete === undefined) {
      return (<span className="unfinished-label">unfinished</span>)
    } else {
      return (<span className="no-live-judge-label">no live judge</span>)
    }
  } else {
    return makeProbabilityBar("judgment-bar-small", [probCorrect, 1.0 - probCorrect], ["judgment-bar-correct", "judgment-bar-incorrect"]);
  }
}

function includesIgnoreCase(str: string, substr: string) {
  return str.toLowerCase().includes(substr.toLowerCase());
}

// TODO filter on outcomes
function includeDebate(filters: Filters, debate: DebateMetadata) {
  // return (filters.human || !debate.setting.isHuman)
  const statusLabel = getStatusLabel(debate.status);
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
    filters.searchQuery.split(" ").every((word) => {
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

function makeHeaderCell(
  name: string,
  sortColumn: string, setSortColumn: (sortColumn: string) => void,
  sortReversed: boolean, setSortReversed: (sortReversed: boolean) => void
) {
  const className = "table-header" + ((sortColumn === name) ? " table-header-selected" : "")
  return (
    <th className={className} scope="col" onClick={(event) => {
      if (sortColumn === name) {
        setSortReversed(!sortReversed);
      } else {
        setSortReversed(false);
      }
      setSortColumn(name);
    }}>
      <div className="flex-row">
        <span className="flex-grow">{name}</span>
        <span className={sortColumn === name ? "" : "hidden"}>
          {(sortReversed ? " ▼" : " ▲")}
        </span>
      </div>
    </th >
  )
}

function getStringField(column: string) {
  if (column === "Room Name") {
    return (debate: DebateMetadata) => debate.name.toString();
  } else if (column === "Debaters") {
    return (debate: DebateMetadata) => debate.setting.isHuman ? "Human" : "Consultancy"
  } else if (column === "Format") {
    return (debate: DebateMetadata) => debate.setting.isDebate ? "Debate" : "Consultancy";
  } else if (column === "Outcome") {
    return (debate: DebateMetadata) => getStatusLabel(debate.status);
  } else if (column === "Judge") {
    return (debate: DebateMetadata) => debate.ultimateRoles.Judge;
  } else if (column === "Honest") {
    return (debate: DebateMetadata) => debate.ultimateRoles['Honest Debater'];
  } else if (column === "Dishonest") {
    return (debate: DebateMetadata) => debate.ultimateRoles['Dishonest Debater'];
  } else {
    return (debate: DebateMetadata) => debate.name.toString();
  }
}

function compareDebatesByColumn(debate1: DebateMetadata, debate2: DebateMetadata, column: string) {
  if (column == "Outcome") {
    const status1 = debate1.status;
    const status2 = debate2.status;
    const judgingInfo1 = status1.Complete?.result.judgingInfo
    const judgingInfo2 = status2.Complete?.result.judgingInfo
    const judgment1 = judgingInfo1?.finalJudgement
    const judgment2 = judgingInfo2?.finalJudgement
    const probCorrect1 = judgment1?.[(judgingInfo1 as JudgingResult).correctAnswerIndex];
    const probCorrect2 = judgment2?.[(judgingInfo2 as JudgingResult).correctAnswerIndex];
    if (probCorrect1 === undefined) {
      if (probCorrect2 === undefined) {
        return 0;
      } else {
        return 1;
      }
    } else {
      if (probCorrect2 === undefined) {
        return -1;
      } else {
        return probCorrect2 - probCorrect1;
      }
    }
  } else {
    const getField = getStringField(column)
    const field1 = (getField(debate1) || "").toString()
    const field2 = (getField(debate2) || "").toString()
    if (field1 === null) {
      if (field2 === null) {
        return 0;
      } else {
        return 1;
      }
    } else {
      if (field2 === null) {
        return -1;
      } else {
        return field1.localeCompare(field2);
      }
    }
  }
}

function LoadedDebatesTable(filters: Filters, debates: DebateMetadata[]) {
  const includedDebates = debates.filter((debate) => includeDebate(filters, debate)).sort(
    (debate1, debate2) => compareDebatesByColumn(debate1, debate2, filters.sortColumn) * (filters.sortReversed ? -1 : 1)
  )
  return (
    <div>
      Showing {numLabel("room", includedDebates.length)}.
      <table className="table">
        <thead><tr>
          <th scope="col">#</th>
          {makeHeaderCell("Room Name", filters.sortColumn, filters.setSortColumn, filters.sortReversed, filters.setSortReversed)}
          {makeHeaderCell("Debaters", filters.sortColumn, filters.setSortColumn, filters.sortReversed, filters.setSortReversed)}
          {makeHeaderCell("Format", filters.sortColumn, filters.setSortColumn, filters.sortReversed, filters.setSortReversed)}
          {makeHeaderCell("Outcome", filters.sortColumn, filters.setSortColumn, filters.sortReversed, filters.setSortReversed)}
          {makeHeaderCell("Judge", filters.sortColumn, filters.setSortColumn, filters.sortReversed, filters.setSortReversed)}
          {makeHeaderCell("Honest", filters.sortColumn, filters.setSortColumn, filters.sortReversed, filters.setSortReversed)}
          {makeHeaderCell("Dishonest", filters.sortColumn, filters.setSortColumn, filters.sortReversed, filters.setSortReversed)}
        </tr></thead>
        <tbody>
          {/* <TransitionGroup
          transitionName="table-row-anim"
          transitionEnterTimeout={500}
          transitionLeaveTimeout={300}> */}
          {includedDebates.map((debate, index) => {
            return (<tr key={debate.name.toString()}>
              <td>{index + 1}</td>
              <td><Link to={"/" + debate.name}>{debate.name}</Link></td>
              {/* <td><a href="#" onClick={(event) => filters.setRoom(debate.name.toString())}>{debate.name}</a></td> */}
              <td>{debate.setting.isHuman ? "Human" : "AI"}</td>
              <td>{debate.setting.isDebate ? "Debate" : "Consultancy"}</td>
              <td className="status-display-cell">{getStatusDisplay(debate.status)}</td>
              <td>{roleLabel("Judge", debate)}</td>
              <td>{roleLabel("Honest Debater", debate)}</td>
              <td>{roleLabel("Dishonest Debater", debate)}</td>
            </tr>)
          })}
          {/* </TransitionGroup> */}
        </tbody>
      </table >
    </div>
  )
}

function DebatesTable(props: Filters) {
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
    return LoadedDebatesTable(props, metadata);
  }
}

function App() {
  // hooks for filter state

  // human, AI, debate, and consultancy checkbox state hooks
  const [human, setHuman] = useSessionStorage("human", true);
  const [ai, setAI] = useSessionStorage("ai", true);
  const [debate, setDebate] = useSessionStorage("debate", true);
  const [consultancy, setConsultancy] = useSessionStorage("consultancy", true);

  // roles
  const [judge, setJudge] = useSessionStorage("judge", true);
  const [honest, setHonest] = useSessionStorage("honest", true);
  const [dishonest, setDishonest] = useSessionStorage("dishonest", true);

  // correct / incorrect checkbox state hooks
  const [correct, setCorrect] = useSessionStorage("correct", true);
  const [incorrect, setIncorrect] = useSessionStorage("incorrect", true);
  const [tied, setTied] = useSessionStorage("tied", true);
  const [unfinished, setUnfinished] = useSessionStorage("unfinished", false);
  const [noLiveJudge, setNoLiveJudge] = useSessionStorage("noLiveJudge", false);

  // included in paper analysis
  const [included, setIncluded] = useSessionStorage("included", true);
  const [excluded, setExcluded] = useSessionStorage("excluded", false);

  // search terms hook
  const [searchQuery, setSearchQuery] = useSessionStorage("searchQuery", "");

  // sort table by column
  const [sortColumn, setSortColumn] = useSessionStorage("sortColumn", "Room Name");
  const [sortReversed, setSortReversed] = useSessionStorage("sortReversed", false);

  const [helpModalActive, setHelpModalActive] = React.useState(false);

  // const [roomOpt, setRoomOpt] = React.useState<string | undefined>(undefined);

  // if (roomOpt !== undefined) {
  //   return (
  //     // <Room roomName={roomOpt} exit={() => setRoomOpt(undefined)} />
  //     <Room roomName={roomOpt} />
  //   );
  // } else {
  return (
    <div className="container">
      <div id="help-modal" tabIndex={-1} role="dialog" className={"modal fade show"} style={{ display: "block", position: "initial" }}>
        <Modal show={helpModalActive} onHide={() => setHelpModalActive(false)} size='lg'>
          <Modal.Header closeButton>
            <Modal.Title>About this interface</Modal.Title>
          </Modal.Header>
          <Modal.Body>
            <p>
              This is the data browser for the paper:
              <div className="p-3">
                <h6 className="mb-1"><a href="https://arxiv.org/abs/2311.08702">Debate Helps Supervise Unreliable Experts</a>.</h6>
                Julian Michael,* Salsabila Mahdi,* David Rein,* Jackson Petty, Julien Dirani,
                Vishakh Padmakumar, and Samuel R. Bowman.
              </div>
              The paper is currently a preprint on arXiv and has not yet been peer-reviewed (* denotes equal contribution).
            </p>

            <p>
              This interface allows you to browse all of the debates we collected as part of our experiments,
              including debates which were left unfinished or filtered out of the data analysis.
              Filters for the data are described below.
            </p>
            <h4>Setting</h4>
            <p>We tested four settings: human debate, human consultancy, AI debate, and AI consultancy.</p>
            <ul>
              <li><b>Human/AI:</b> whether the debaters or consultant in the round were human or AI. (Judges are always humans.)</li>
              <li><b>Debate/Consultancy:</b> whether both answers had expert advocates (debate) or just one (consultancy).</li>
            </ul>
            <h4>Outcome</h4>
            <ul>
              <li><b>Correct/Incorrect/Tied:</b> whether the judge gave more than, less than, or
                exactly 50% probability to the correct answer at the end of the debate.</li>
              <li><b>Unfinished:</b> some debates were never finished before our data collection wrapped up.
                We include them for completeness.
              </li>
              <li><b>No live judge:</b> In some debates (not included in the final analysis),
                we had no live judge and ended the debate by mutual agreement between the debaters.
                Check this to include these debates as well.
              </li>
            </ul>
            <h4>Roles</h4>
            <ul>
              <li><b>Honest:</b> includes debates (which all have an honest debater) and consultancies with an honest consultant.</li>
              <li><b>Dishonest:</b> includes debates (which all have a dishonest debater) and consultancies with a dishonest consultant.</li>
              <li><b>Judge:</b> includes debates with a live judge.</li>
            </ul>
            <h4>Used in the paper</h4>
            <p>
              This filter allows you to look at debates and consultancies which were filtered out of the final analysis.
              There were a few reasons a round might have been excluded:
            </p>
            <ul>
              <li>We removed rounds over questions which were marked by QuALITY annotators to
                require very little story context to answer, as we stopped gathering data on these questions partway through
                based on the assumption that they would be too easy.
              </li>
              <li>
                We excluded some consultancies in order to make sure that honest and dishonest consultancies were
                evenly represented in the final analysis, since the consultancy accuracy measure assumes a 50% chance of
                the consultant being honest.
              </li>
            </ul>
          </Modal.Body>
        </Modal>
      </div >
      <header>
        <h1>Debate Helps Supervised Unreliable Experts</h1>
        <p>This is the data browser for <a href="https://arxiv.org/abs/2311.08702">the paper</a> of
          the above title
          (<a href="https://github.com/julianmichael/debate/tree/2023-nyu-experiments">code</a>, <a href="https://github.com/julianmichael/debate/tree/2023-nyu-experiments/pub">data</a>).
        </p>
        <p>
          <a href="/#" onClick={() => setHelpModalActive(!helpModalActive)}>» About this interface «</a>
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
        included={included} excluded={excluded}
        searchQuery={searchQuery}
        sortColumn={sortColumn} setSortColumn={setSortColumn}
        sortReversed={sortReversed} setSortReversed={setSortReversed}
      // setRoom={setRoomOpt}
      />
    </div >
  );
  // }
}

export default App;
