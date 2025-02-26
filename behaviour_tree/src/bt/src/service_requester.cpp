#include "service_requester.hpp"

SendServiceRequester::SendServiceRequester( 
  const std::string& name, 
  const BT::NodeConfig& config,
  const BT::RosNodeParams& params
)
: BT::RosServiceNode<BoolSrv>(name, config, params) 
{}


bool 
SendServiceRequester::setRequest(
  BoolSrv::Request::SharedPtr & request
) 
{
  if(getInput<std::string>("trigger") == "true"){
    request->data = true;
  } 
  else {
    request->data = false;
  }
  return true;
}


BT::NodeStatus 
SendServiceRequester::onResponseReceived(
  const BoolSrv::Response::SharedPtr & response
) 
{
  if(response->success){
    return BT::NodeStatus::SUCCESS;
  }
  else {
    return BT::NodeStatus::FAILURE;
  }
}

 
BT::NodeStatus 
SendServiceRequester::onFailure(
  BT::ServiceNodeErrorCode error
) 
{
  // Apparently weak_ptr does not have a -> operator, so one has to lock it to retrieve the shared_ptr
  std::shared_ptr<rclcpp::Node> node = this->node_.lock();
  if (node){
    RCLCPP_ERROR(node->get_logger(), "Service failed with error code %d", error);
  }
  return BT::NodeStatus::FAILURE;
}