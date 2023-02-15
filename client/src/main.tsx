import {createRoot} from 'react-dom/client';
import React from 'react';
import App from "./App"
import {ChakraProvider} from '@chakra-ui/react'

const container = document.getElementById('react-root');
if (container !== null) {
    const root = createRoot(container as HTMLElement);
    root.render(<ChakraProvider><App/></ChakraProvider>);
}
