export const fetchData = async () => {
    let response = await fetch('/api/data')
    if (!response.ok) {

        throw new Error('Network response was not ok')

    }
    let data = await response.json()
    return data
}