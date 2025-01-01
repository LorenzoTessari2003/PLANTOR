#include "sleep_node.hpp"


SleepNode::SleepNode(const std::string& name, const BT::NodeConfig& config)
  : BT::StatefulActionNode(name, config)
{}


BT::NodeStatus SleepNode::onStart()
{
  int msec = 0;
  getInput("msec", msec);

  if( msec <= 0 ) {
    // No need to go into the RUNNING state
    return BT::NodeStatus::SUCCESS;
  }
  else {
    // once the deadline is reached, we will return SUCCESS.
    deadline_ = system_clock::now() + milliseconds(msec);
    return BT::NodeStatus::RUNNING;
  }
}


/// method invoked by an action in the RUNNING state.
BT::NodeStatus SleepNode::onRunning() 
{
  if ( system_clock::now() >= deadline_ ) {
    return BT::NodeStatus::SUCCESS;
  }
  else {
    return BT::NodeStatus::RUNNING;
  }
}


void SleepNode::onHalted()
{
  // nothing to do here...
  std::cout << "SleepNode interrupted" << std::endl;
}
