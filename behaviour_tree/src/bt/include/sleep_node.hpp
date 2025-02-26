#pragma once

#include <chrono>

#include "behaviortree_cpp/bt_factory.h"

using namespace std::chrono;

// Example of Asynchronous node that uses StatefulActionNode as base class
class SleepNode : public BT::StatefulActionNode
{
private:
  system_clock::time_point deadline_;

public:
  SleepNode(const std::string& name, const BT::NodeConfig& config);

  static BT::PortsList providedPorts()
  {
    // amount of milliseconds that we want to sleep
    return{ BT::InputPort<int>("msec") };
  }

  BT::NodeStatus onStart() override;

  /// method invoked by an action in the RUNNING state.
  BT::NodeStatus onRunning() override;

  void onHalted() override;
};