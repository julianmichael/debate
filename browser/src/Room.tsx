import React from 'react';
import { useEffect, useState } from 'react';
import './Room.css';

interface Props {
    name: string;
    exit: () => void;
}

function Room(props: Props) {
    return (
        <div className="container">
            <button onClick={(event) => props.exit()}>Back</button>
            Room: {props.name}
        </div>
    );
}

export default Room;
