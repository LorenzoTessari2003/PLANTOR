// api.js

const API_BASE_URL = process.env.REACT_APP_API_BASE_URL || 'http://localhost:5000/api';

/**
 * Validate high-level and low-level descriptions.
 * @param {string} highLevel - High-level description text.
 * @param {string} lowLevel - Low-level description text.
 * @returns {Promise<Object>} API response.
 */
export const validateDescriptions = async (highLevel, lowLevel) => {
    const response = await fetch(`${API_BASE_URL}/validate`, {
        method: 'POST',
        headers: { 
            'Content-Type': 'application/json',
        },
        body: JSON.stringify({ highLevel: highLevel, lowLevel: lowLevel }),
    });

    if (!response.ok) {
        const error = await response.json();
        throw new Error(error.error);
    }

    return response.json();
};

/**
 * Generate the high-level knowledge base.
 * @param {string} highLevel - High-level description text.
 * @returns {Promise<Object>} API response.
 */
export const generateHighLevelKB = async (highLevelDesc) => {
    const response = await fetch(`${API_BASE_URL}/generate_hl_kb`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ description: highLevelDesc }),
    });

    if (!response.ok) {
        const error = await response.json();
        throw new Error(error.error);
    }

    return response.json();
};

/**
 * Generate the low-level knowledge base.
 * @param {string} lowLevel - Low-level description text.
 * @returns {Promise<Object>} API response.
 */
export const generateLowLevelKB = async (lowLevelDesc, hlkbContent, hlInitContent, hlGoalContent, hlActionsContent) => {
    const response = await fetch(`${API_BASE_URL}/generate_ll_kb`, {
        method: 'POST',
        // mode: 'cors',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ 
            lowLevelDesc: lowLevelDesc,
            hlkbContent: hlkbContent,
            hlInitContent: hlInitContent,
            hlGoalContent: hlGoalContent,
            hlActionsContent: hlActionsContent
        }),
    });

    if (!response.ok) {
        const error = await response.json();
        throw new Error(error.error);
    }

    return response.json();
};

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Generate the behavior tree in XML format.
 * @param {string} highLevelKB - High-level knowledge base.
 * @param {string} lowLevelKB - Low-level knowledge base.
 * @returns {Promise<Object>} API response.
 */
export const generateBehaviorTree = async (lowLevelKB) => {
    const response = await fetch(`${API_BASE_URL}/generate_bt`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ low_level_kb: lowLevelKB }),
    });

    if (!response.ok) {
        const error = await response.json();
        console.log("Something went wrong in generateBehaviorTree");
        sleep(5000);
        throw new Error(error.error);
    }

    sleep(5000);

    return response.json();
};

