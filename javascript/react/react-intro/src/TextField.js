import React from 'react'
import style from './style'

const TextField = ({name, onChange, onBlur, error, label}) => (
    <div style={style.inpurtGroup}>
        <label>
            {label}
            <input
                style={style.input}
                type="text"
                name={name}
                onChange={onChange}
                onBlur={onBlur}
            />
            {error && <div style={style.error}>{error}</div>}
        </label>
    </div>
);

export default TextField;
