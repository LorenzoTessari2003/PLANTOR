// Home.jsx

// import '../components/button.css'
import React, { useState } from 'react';
import { validateDescriptions, generateHighLevelKB, generateLowLevelKB, generateBehaviorTree } from '../api';
import './spinner.css';

const Home = () => {
    // State to track if the page is loading
    const [isLoading, setIsLoading] = useState(false);

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
    const [hlInitContent, setHLInitContent] = useState('');
    const [hlGoalContent, setHLGoalContent] = useState('');
    const [hlActionsContent, setHLActionsContent] = useState('');

    // State for the generated low-level KB content
    const [llkbContent, setLLKBContent] = useState('');
    const [llInitContent, setLLInitContent] = useState('');
    const [llGoalContent, setLLGoalContent] = useState('');
    const [llActionsContent, setLLActionsContent] = useState('');
    const [llMappingsContent, setLLMappingsContent] = useState('');

    // State to track if planning has passed or failed
    const [planningError, setPlanningError] = useState('');

    const [btXML, setBtXML] = useState('');

    const [visBT, setVisBT] = useState(false);

    const handleValidation = async () => {
        try {
            setIsLoading(true);
            setValidationError('');
            const result = await validateDescriptions(highLevelDesc, lowLevelDesc);
            if (result.isValid) {
                setShowGenerateHLKB(true);
            }
        } catch (error) {
            setValidationError(error.message);
        }
        finally {
            setIsLoading(false);
        }
    };

    const handleGenerateHlKB = async () => {
        try {
            setIsLoading(true);
            const result = await generateHighLevelKB(highLevelDesc);
            setHLKBContent(result.kb);
            setHLInitContent(result.init);
            setHLGoalContent(result.goal);
            setHLActionsContent(result.actions);
        } catch (error) {
            console.error(error);
        }
        finally {
            setIsLoading(false);
        }
    };

    const handleGenerateLlKB = async () => {
        try {
            setIsLoading(true);
            const result = await generateLowLevelKB(lowLevelDesc, hlkbContent, hlInitContent, hlGoalContent, hlActionsContent);
            console.log(result);
            setLLKBContent(result.kb);
            setLLInitContent(result.init);
            setLLGoalContent(result.goal);
            setLLActionsContent(result.actions);
            setLLMappingsContent(result.mappings);
            console.log(llkbContent, llInitContent, llGoalContent, llActionsContent, llMappingsContent);
        } catch (error) {
            console.error(error);
        }
        finally {
            setIsLoading(false);
        }
    };

    const handleGenerateBt = async () => {
        try {
            setIsLoading(true);
            const result = await generateBehaviorTree(hlkbContent, llkbContent);
            console.log("Got BT: ", result);
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
        finally {
            setIsLoading(false);
        }
    };

    const handleCheckBT = () => {
        const path = 'BT.html';
        window.open(path, '_blank');
        setVisBT(true);
    };

    const handleExecute = () => {
        const path = 'BT2.html';
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
                    <div className="text-center mt-3">
                        <div className="spinner-button-container">
                            {isLoading && (
                                <div className="spinner-with-text">
                                    <div className="spinner"></div>
                                    <div className="loading-text">This may take a few seconds...</div>
                                </div>
                            )}
                            <button className="btn btn-primary" onClick={handleValidation}>
                                Validate Options
                            </button>
                        </div>
                    </div>
            )}

            {/* Display Validation Error if Validation Fails */}
            {validationError && (
                <div className="alert alert-danger text-center mt-2" role="alert">
                    {validationError}
                </div>
            )}

            {/* Generate HL KB Button and Text Area */}
            {showGenerateHLKB && (
                <div className="text-center mt-3 mb-4">
                    <div className="spinner-button-container">
                        {isLoading && (
                            <div className="spinner-with-text">
                                <div className="spinner"></div>
                                <div className="loading-text">This may take a few seconds...</div>
                            </div>
                        )}
                        <button className="btn btn-success" onClick={handleGenerateHlKB}>
                            Generate HL KB
                        </button>
                    </div>
                </div>
                // <div className="text-center">
                //     <button className="btn btn-success mb-3 mt-2" onClick={handleGenerateHlKB}>
                //         Generate HL KB
                //     </button>
                // </div>
            )}

            {hlkbContent && hlInitContent && hlGoalContent && hlActionsContent && (
            <div>
                <div className="row">
                    <div className="col-md-6">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                HL Init State
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={hlInitContent}
                                    onChange={(e) => setHLInitContent(e.target.value)}
                                    placeholder="This is the HL init state"
                                ></textarea>
                            </div>
                        </div>
                    </div>

                    <div className="col-md-6">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                HL Goal State
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={hlGoalContent}
                                    onChange={(e) => setHLGoalContent(e.target.value)}
                                    placeholder="This is the HL goal state"
                                ></textarea>
                            </div>
                        </div>
                    </div>
                    
                    <div className="col-md-4">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                HL KB
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={hlkbContent}
                                    onChange={(e) => setHLKBContent(e.target.value)}
                                    placeholder="This is the HL KB"
                                ></textarea>
                            </div>
                        </div>
                    </div>

                    <div className="col-md-8">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                HL Actions
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={hlActionsContent}
                                    onChange={(e) => setHLActionsContent(e.target.value)}
                                    placeholder="These are the HL actions"
                                ></textarea>
                            </div>
                        </div>
                    </div>
                    {/* <div className="text-center">
                        <button className="btn btn-success mt-3" onClick={handleGenerateLlKB}>
                            Generate LL KB
                        </button>
                    </div> */}
                    <div className="text-center mt-3 mb-4">
                        <div className="spinner-button-container">
                            {isLoading && (
                                <div className="spinner-with-text">
                                    <div className="spinner"></div>
                                    <div className="loading-text">This may take a few seconds...</div>
                                </div>
                            )}
                            <button className="btn btn-success" onClick={handleGenerateLlKB}>
                                Generate LL KB
                            </button>
                        </div>
                    </div>
                </div>
                <br/>
            </div>
            )}


            {llkbContent && llInitContent && llGoalContent && llActionsContent && llMappingsContent && (
                <div className="row">
                    <div className="col-md-4">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                LL Init State
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={llInitContent}
                                    onChange={(e) => setLLInitContent(e.target.value)}
                                    placeholder="This is the LL init state"
                                ></textarea>
                            </div>
                        </div>
                    </div>

                    <div className="col-md-4">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                LL Goal State
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={llGoalContent}
                                    onChange={(e) => setLLGoalContent(e.target.value)}
                                    placeholder="This is the LL goal state"
                                ></textarea>
                            </div>
                        </div>
                    </div>
                    
                    <div className="col-md-4">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                LL KB
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={llkbContent}
                                    onChange={(e) => setLLKBContent(e.target.value)}
                                    placeholder="This is the LL KB"
                                ></textarea>
                            </div>
                        </div>
                    </div>

                    <div className="col-md-6">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                LL Actions
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={hlActionsContent}
                                    onChange={(e) => setLLActionsContent(e.target.value)}
                                    placeholder="These are the LL actions"
                                ></textarea>
                            </div>
                        </div>
                    </div>

                    <div className="col-md-6">
                        <div className="card shadow p-2 mb-4">
                            <div className="card-header bg-success text-white text-center">
                                LL Mappings
                            </div>
                            <div className="card-body">
                                <textarea
                                    className="form-control"
                                    rows="6"
                                    value={llMappingsContent}
                                    onChange={(e) => setLLMappingsContent(e.target.value)}
                                    placeholder="These are the LL mappings"
                                ></textarea>
                            </div>
                        </div>
                    </div>
                    {/* <div className="text-center">
                        <button className="btn btn-success mt-3" onClick={handleGenerateBt}>
                            Plan
                        </button>
                    </div> */}

                    <div className="text-center mt-3 mb-4">
                        <div className="spinner-button-container">
                            {isLoading && (
                                <div className="spinner-with-text">
                                    <div className="spinner"></div>
                                    <div className="loading-text">This may take a few seconds...</div>
                                </div>
                            )}
                            <button className="btn btn-success" onClick={handleGenerateBt}>
                                Plan
                            </button>
                        </div>
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
                <div>
                    <div className="card shadow p-3 mt-3">
                        <div className="card-header bg-success text-white">
                            Behavior Tree
                        </div>
                        <div className="card-body" style={{ border: '1px solid lightgray' }}>
                            <pre>{btXML}</pre>
                        </div>
                    </div>
                    {!visBT && (
                        <div className="text-center mt-3">
                            <button className="btn btn-success" onClick={handleCheckBT}>
                                Check BT
                            </button>
                        </div>
                    )}
                    {visBT && (
                        <div className="text-center mt-3">
                            <button className="btn btn-danger" onClick={handleExecute}>
                                Execute
                            </button>
                        </div>
                    )}
                </div>
            )}
            {/* {btXML && (
                <div className="card-body">
                    <iframe
                        src="BT.html"
                        title="Behavior Tree"
                        width="100%"
                        height="500px"
                        style={{ border: '1px solid lightgray' }}
                    ></iframe>
                </div>
            )} */}

            <div><br/></div>
        </div>
    );
};

export default Home;
