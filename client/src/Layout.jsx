import React, { useState, useRef } from "react"

const Layout = ({ left, middle, right }) => {
    const [leftWindowSize, setLeftWidth] = useState(200);
    const [rightWindowSize, setRightWidth] = useState(300);
    const [resizer, setResizer] = useState(null);
    const myref = useRef(null);
    return (
        <div className="h-full flex"
            ref={myref}
            onMouseUp={(e) => {
                setResizer(null);
                myref.current.style.cursor = 'auto'
            }}
            onMouseMove={(e) => {
                e.preventDefault()
                if (resizer === 'left') {
                    if (e.clientX !== 0) setLeftWidth(e.clientX)
                    myref.current.style.cursor = 'ew-resize';
                } else if (resizer === 'right') {
                    if (e.clientX !== 0) setRightWidth(document.body.clientWidth
                        - e.pageX - 10)
                    myref.current.style.cursor = 'ew-resize';
                }
            }}>
            <div style={{ width: leftWindowSize }}>
                {left}
            </div>
            <div
                style={{ cursor: 'ew-resize' }}
                onMouseDown={(e) => {
                    e.preventDefault()
                    setResizer('left')
                }}
                className="h-full bg-black w-2"></div>
            <div className="flex-grow overflow-x-auto overflow-y-hidden">
                {middle}
            </div>
            <div
                style={{ cursor: 'ew-resize' }}
                onMouseDown={(e) => {
                    e.preventDefault()
                    setResizer('right')}}
                className="h-full bg-black w-2"></div>
            <div style={{ width: rightWindowSize }}>
                {right}
            </div>
        </div>)
}

export default Layout