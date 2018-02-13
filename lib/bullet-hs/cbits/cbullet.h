#ifndef CBULLET_H
#define CBULLET_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

  // poor man's newtype.  Dear God.
  typedef struct broadphase_interface { char unused; } broadphase_interface;
  typedef struct collision_configuration { char unused; } collision_configuration;
  typedef struct collision_dispatcher { char unused; } collision_dispatcher;
  typedef struct constraint_solver { char unused; } constraint_solver;
  typedef struct dynamics_world { char unused; } dynamics_world;
  typedef struct motion_state { char unused; } motion_state;
  typedef struct static_plane_shape { char unused; } static_plane_shape;
  typedef struct sphere_shape { char unused; } sphere_shape;
  typedef struct box_shape { char unused; } box_shape;
  typedef struct collision_shape { char unused; } collision_shape;
  typedef struct rigid_body_construction_info { char unused; } rigid_body_construction_info;
  typedef struct rigid_body { char unused; } rigid_body;
  typedef struct transform { char unused; } transform;
  typedef struct collision_object { char unused; } collision_object;
  typedef struct typed_constraint { char unused; } typed_constraint;
  typedef struct serializer { char unussed; } serializer;
  typedef float scalar;

  broadphase_interface* new_broadphase_interface();
  void free_broadphase_interface(broadphase_interface* broadphase);

  collision_configuration* new_default_collision_configuration();
  void free_collision_configuration(collision_configuration* collision_configuration);

  collision_dispatcher* new_collision_dispatcher(collision_configuration* collision_configuration);
  void free_collision_dispatcher(collision_dispatcher* collision_dispatcher);

  constraint_solver* new_sequential_impulse_constraint_solver();
  void free_constraint_solver(constraint_solver* constraint_solver);

  // btDiscreteDynamicsWorld
  dynamics_world* new_discrete_dynamics_world (collision_dispatcher* dispatcher, 
					       broadphase_interface* broadphase, 
					       constraint_solver* solver, 
					       collision_configuration* collision_configuration);
  void free_dynamics_world(dynamics_world* world);
  void set_gravity(dynamics_world* world, scalar x, scalar y, scalar z);
  int step_simulation(dynamics_world* world,
		      scalar time_step,
		      const int* max_sub_steps,
		      const scalar* fixed_time_step);
  //  int default_step_simulation(discrete_dynamics_world* world, scalar time_step);
  void add_rigid_body(dynamics_world* world, rigid_body* rigid_body);
  int get_num_collision_objects(dynamics_world* world);
  collision_object* get_collision_object(dynamics_world* world, int idx);
  void remove_collision_object(dynamics_world* world, collision_object* obj);
  int get_num_constraints(dynamics_world* world);
  typed_constraint* get_constraint(dynamics_world* world, int idx);
  void add_constraint(dynamics_world* world, typed_constraint* constriant);
  void remove_constraint(dynamics_world* world, typed_constraint* constraint);
  void serialize(dynamics_world* world, serializer* serializer);

  //btCollisionObject
  rigid_body* collision_object_to_rigid_body(collision_object* obj);
  transform* co_allocate_world_transform(collision_object* obj);

  // btDefaultMotionState
  motion_state* new_default_motion_state(transform* transform);
  void free_motion_state(motion_state* motion_state);
  transform* ms_allocate_world_transform(motion_state* motion_state);

  // byRigidBodyContructionInfo
  rigid_body_construction_info* new_rigid_body_construction_info
  (scalar mass,
   motion_state* motion_state,
   collision_shape* collision_shape,
   scalar inertia_x,
   scalar inertia_y,
   scalar inertia_z);
  void free_rigid_body_construction_info(rigid_body_construction_info* rbci);

  // btRigidBody
  rigid_body* new_rigid_body(rigid_body_construction_info* rbci);
  void free_rigid_body(rigid_body* rigid_body);
  motion_state* rb_get_motion_state(rigid_body* rigid_body);
  int is_static_object(rigid_body* rigid_body); // returns bool
  int is_kinematic_object(rigid_body* rigid_body); // returns bool
  void set_activation_state(rigid_body* rigid_body, int new_state);
  transform* get_center_of_mass_transform(rigid_body* rigid_body);

  // btCollisionShape
  void calculate_local_inertia(collision_shape* collision_shape,
			       scalar mass,
			       scalar* x_out,
			       scalar* y_out,
			       scalar* z_out);
  void free_collision_shape(collision_shape* collision_shape);

  // btStaticPlaneShape
  static_plane_shape* new_static_plane_shape(scalar x,
					     scalar y,
					     scalar z,
					     scalar plane_constant);
  void free_static_plane_shape(static_plane_shape* static_plane_shape);

  // btSphereShape
  sphere_shape* new_sphere_shape(scalar radius);
  void free_sphere_shape(sphere_shape* sphere_shape);

  // btBoxShape
  box_shape* new_box_shape(scalar half_x, scalar half_y, scalar half_z);
  void free_box_shape(box_shape* box_shape);

  // btTransform
  transform* new_transform(scalar r, // quaternion
			   scalar i,
			   scalar j,
			   scalar k,
			   scalar x, // vector
			   scalar y,
			   scalar z);
  void free_transform(transform* transform);
  void get_origin(transform* transform, scalar* x_out, scalar* y_out, scalar* z_out);
  void set_origin(transform* transform, scalar x, scalar y, scalar z);
  void set_identity(transform* transform);

  // btTypedConstraint
  void free_typed_constraint(typed_constraint* constraint);

  // btPoint2PointConstraint
  typed_constraint* new_point2point_constraint(rigid_body* rigid_body,
					       scalar pivot_x,
					       scalar pivot_y,
					       scalar pivot_z);

  // btDefaultSerializer
  serializer* new_default_serializer();
  void free_serializer(serializer* serializer);
  const unsigned char* get_buffer_pointer(serializer* serializer);
  int get_current_buffer_size(serializer* serializer);

  // btKinematicCharacterController
  kinematic_character_controller* new_kinematic_character_controller
  (pair_caching_ghost_object* ghost_object,
   convex_shape* convex_shape,
   scalar step_height);
  void free_kinematic_character_controller(kinematic_character_controller* kcc);
  
#ifdef __cplusplus
}
#endif

#endif
