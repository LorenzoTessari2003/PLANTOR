// Home.jsx

// import '../components/button.css'
import React, { useState } from 'react';
import { validateDescriptions, generateHighLevelKB, generateLowLevelKB, generateBehaviorTree } from '../api';


const Home = () => {
    // State for the high-level description text area
    const [highLevelDesc, setHighLevelDesc] = useState('');
    
    // State for the low-level description text area
    const [lowLevelDesc, setLowLevelDesc] = useState('');

    // State to track if validation has passed or failed
    const [validationError, setValidationError] = useState(null);
    
    // State to track if the high-level KB is ready to be generated
    const [showGenerateHLKB, setShowGenerateHLKB] = useState(false);

    // State for the generated high-level KB content
    const [hlkbContent, setHLKBContent] = useState('');

    // State for the generated low-level KB content
    const [llkbContent, setLLKBContent] = useState('');

    // State to track if planning has passed or failed
    const [planningError, setPlanningError] = useState('');

//     const [llKB, setLlKB] = useState('');
    const [btXML, setBtXML] = useState('');
//     const [currentStep, setCurrentStep] = useState(1); // Steps: 1=Input, 2=HL KB, 3=LL KB, 4=BT

    const handleValidation = async () => {
        try {
            setValidationError('');
            const result = await validateDescriptions(highLevelDesc, lowLevelDesc);
            console.log(result);
            if (result.isValid) {
                setShowGenerateHLKB(true);
            }
        } catch (error) {
            setValidationError(error.message);
        }
    };

    const handleGenerateHlKB = async () => {
        try {
            const result = await generateHighLevelKB(highLevelDesc);
            setHLKBContent(result.kb);
        } catch (error) {
            console.error(error);
        }
    };

    const handleGenerateLlKB = async () => {
        try {
            const result = await generateLowLevelKB(lowLevelDesc, hlkbContent);
            setLLKBContent(result.kb);
        } catch (error) {
            console.error(error);
        }
    };

    const handleGenerateBt = async () => {
        try {
            const result = await generateBehaviorTree(hlkbContent, llkbContent);
            if (result.bt_error) {
                setPlanningError(result.bt_error);
                setBtXML('');
            }
            else{
                setPlanningError('');
                setBtXML(result.behavior_tree);
            }
        } catch (error) {
            console.error(error);
        }
    };

    return (
        <div className="container mt-4">
            <div className="row">
                {/* High-Level Description Card */}
                <div className="col-md-6">
                    <div className="card shadow p-3 mb-4">
                        <div className="card-header bg-primary text-white text-center">
                            High-Level Description
                        </div>
                        <div className="card-body">
                            <textarea
                                className="form-control"
                                rows="6"
                                value={highLevelDesc}
                                onChange={(e) => setHighLevelDesc(e.target.value)}
                                placeholder="Enter the high-level task description here"
                            ></textarea>
                        </div>
                    </div>
                </div>

                {/* Low-Level Description Card */}
                <div className="col-md-6">
                    <div className="card shadow p-3 mb-4">
                        <div className="card-header bg-primary text-white text-center">
                            Low-Level Description
                        </div>
                        <div className="card-body">
                            <textarea
                                className="form-control"
                                rows="6"
                                value={lowLevelDesc}
                                onChange={(e) => setLowLevelDesc(e.target.value)}
                                placeholder="Enter the low-level task description here"
                            ></textarea>
                        </div>
                    </div>
                </div>
            </div>

            {/* Centered Validate Options Button */}
            {!showGenerateHLKB && (
                <div className="text-center">
                    <button className="btn btn-primary mt-3" onClick={handleValidation}>
                        Validate Options
                    </button>
                </div>
            )}

            {/* Display Validation Error if Validation Fails */}
            {validationError && (
                <div className="alert alert-danger text-center mt-3" role="alert">
                    {validationError}
                </div>
            )}

            {/* Generate HL KB Button and Text Area */}
            {showGenerateHLKB && (
                <div className="text-center mt-4">
                    <button
                        className="btn btn-success mb-3"
                        onClick={handleGenerateHlKB}
                    >
                        Generate HL KB
                    </button>

                    {hlkbContent && (
                        <div>
                            <div className="card shadow p-3 mt-3">
                                <div className="card-header bg-success text-white">
                                    High-Level Knowledge Base
                                </div>
                                <div className="card-body">
                                    <textarea
                                        className="form-control"
                                        rows="6"
                                        value={hlkbContent}
                                        onChange={(e) => setHLKBContent(e.target.value)}
                                    ></textarea>
                                </div>
                            </div>
                            <div className="text-center">
                                <button className="btn btn-success mt-3" onClick={handleGenerateLlKB}>
                                    Generate LL KB
                                </button>
                            </div>
                        </div>
                    )}
                </div>
            )}


            {llkbContent && (
                <div>
                    <div className="card shadow p-3 mt-3">
                        <div className="card-header bg-success text-white">
                            High-Level Knowledge Base
                        </div>
                        <div className="card-body">
                            <textarea
                                className="form-control"
                                rows="6"
                                value={llkbContent}
                                onChange={(e) => setLLKBContent(e.target.value)}
                            ></textarea>
                        </div>
                    </div>
                    <div className="text-center">
                        <button className="btn btn-success mt-3" onClick={handleGenerateBt}>
                            Plan
                        </button>
                    </div>
                </div>
            )}

            {/* Display Planning Error if Planning Fails */}
            {planningError && (
                <div className="alert alert-danger text-center mt-3" role="alert">
                    {planningError}
                </div>
            )}
            
            {/* Display Behavior Tree */}
            {btXML && (
                <div className="card shadow p-3 mt-3">
                    <div className="card-header bg-success text-white">
                        Behavior Tree
                    </div>
                    <div className="card-body" style={{ border: '1px solid lightgray' }}>
                        <pre>{btXML}</pre>
                    </div>
                </div>
            )}
            <div><br/></div>
        </div>
    );
};

export default Home;
