import React, {useEffect} from "react"
import useAppStore from "./state";
import { WrenchIcon, ExclamationTriangleIcon } from '@heroicons/react/24/outline'

const Hint = ({replacement} : any) => {
    if (replacement.kind == "Type hole") {
        return (<div>
            Change <span className={'mx-1'}>{replacement.original}</span> to an instance of <span className={'mx-1'}>{replacement.signature}</span>
        </div>)
    } else if (replacement.kind == "Type wildcard") {
        return (<div>
            Change <span className={'mx-1'}>{replacement.original}</span>  to type <span className={'mx-1'}>{replacement.signature}</span>
        </div>)
    } else {
        return (<div>
            Change <span className={'mx-1'}>{replacement.original}</span>  to a different expression
        </div>)
    }

}
const Fix = ({fix, slices, number}: any) => {
    let activeFix = useAppStore((state : any) => state.fix)
    let chooseFix = useAppStore((state : any) => state.chooseFix)
    let replacements = useAppStore((state : any) => state.replacements)
    let setHighlight = useAppStore((state:any) => state.setHighlight)
    let active = activeFix === fix.setId
    let rules = fix.rules
    let activeSlices = slices.filter(slice => rules.includes(slice.slice_id))

    return (
        <div className={'flex flex-col rounded-sm py-0.5 px-1 cursor-pointer my-0.5 ' + (active ? 'bg-amber-100' : 'bg-stone-100')}>
            <div
                className={'flex items-center'}
                onClick={_ => {
                    chooseFix(fix.setId)
                }}>
                <WrenchIcon className="h-6 w-6 "/>
                <span className={"ml-2"}>Fix {number + 1}</span>
            </div>
            {active ?
                <div className={"text-gray-700"}>This location appears in {activeSlices[0].appears.length} conflicts</div>
                :<></>
            }
            <div>
                {
                    replacements.map(r => activeFix == fix.setId ? <Hint replacement={r} key={r.hole_id}/> : null)
                }
            </div>

        </div>)


}
const Error = ({error} : any) => {
    let setHighlight = useAppStore((state:any) => state.setHighlight)
    return (<div className={"bg-stone-200 p-1 rounded-md flex flex-col mb-2"}>

        <div className={"flex items-center h-10"}>
            <ExclamationTriangleIcon className={"h-6 text-red-400"}/>
            <div className={"ml-2 font-bold h-6 leading-6"}>Type error {error.error_id + 1}</div>
        </div>
        <div className={"flex flex-col bg-white rounded-sm p-1"}>
            <div>The type error contains {error.mus_list.length} minimal conflicts:</div>
            <div className={'flex'}>
            {error.mus_list.map((mus, i) => {
                let slices = error.slices
                let rules = mus.rules
                let activeSlices = slices.filter(slice => rules.includes(slice.slice_id))
                let locations = activeSlices.map(slice => slice.loc)
                return (<div
                onClick={_ => {
                    setHighlight(locations)
                }}
                className={'w-8 bg-amber-100 rounded-sm p-1 mr-1 align-center cursor-pointer'} key={i}>
                {i + 1}
            </div>)})}

            </div>
        </div>

        <div className={'font-bold mt-2'}>Possible Fixes:</div>
        <div className={"flex flex-col bg-white rounded-sm p-1"}>
            {error.mcs_list.map((fix, i) => <Fix fix={fix} number={i} slices={error.slices} key={i}/>)}
        </div>

    </div>)
}
const Debugger = () => {
    const errors = useAppStore((state: any) => state.errors)
    const isLoading = useAppStore((state: any) => state.isLoading)
    if (isLoading) {
        return <div>Type checking in progress</div>
    }
    return (<div className="p-2">
        {errors.map(error => <Error error={error} key={error.error_id}/>)}
    </div>)
};

export default Debugger
