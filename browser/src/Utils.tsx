import React from 'react';

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
                    <div className={styles[index] + " text-nowrap"} style={{ width: pctString }}>
                        <span className="ms-1">{icons ? icons[index] + " " : ""}{pctString}</span>
                    </div>
                );
            })}
        </div>
    )
}